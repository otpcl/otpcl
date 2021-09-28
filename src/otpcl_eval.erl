% @doc OTPCL interpreter/evaluator.
%
% OTPCL's interpreter effectively revolves around repeatedly calling
% 2-arity functions ("commands"), the first argument being the actual
% list of arguments for that function/command, and the second being
% the current interpretation state (expressed as a tuple of two maps,
% one with all command definitions and one with all variable
% definitions).  Each command-backing function in turn returns a tuple
% with a return value and an updated state.
%
% To illustrate: when OTPCL's parser encounters the command invocation
% `foo bar baz' and sends the corresponding parse tree to the
% interpreter, the interpreter in turn calls `{Result, NewState} =
% Fun([bar, baz], State)' (where `Fun' is the value of the `foo' key
% in the first element of the `State' tuple, `Result' is the return
% value for that command, and `NewState' is the updated state).
%
% This means it's pretty straightforward to define an OTPCL command
% yourself from within Erlang: simply define a 2-arity function where
% the first argument is a list and the second argument is a 2-element
% tuple of maps.  A module that defines OTPCL commands can/should
% specify which functions in that module are "OTPCL-aware" in this
% fashion like so:
%
% ```
% -module(my_otpcl_cmds).
% -export([foo/2, bar/2, baz/2]).
% -otpcl_cmds([foo, bar, baz]).  % OTPCL-aware funs in the module
%
% foo([Thing], State) ->
%     {ok, State}.
% bar([Thing1, Thing2], State) ->
%     {{Thing1, Thing2}, State}.
% baz([Name, Val], State) ->
%     otpcl_stdlib:set([Name, Val], State).
% '''
%
% The interpreter itself is also an OTPCL-aware function in this sense
% (albeit with a simplification in that it does not <em>require</em>
% its first argument to be a list; it can take a parse tree directly).
% It can thus be invoked from within OTPCL:
%
% ```
% otpcl> import otpcl_eval
% {ok,otpcl_eval}
% otpcl> eval {set foo "howdy~n"}
% ok
% otpcl> print $foo
% howdy
% ok
% otpcl> import otpcl_env
% ok
% otpcl> eval {
%   ...> set foo "aloha~n"
%   ...> print $foo
%   ...> } [default_state]
% aloha
% [ ... pile of interpreter state ... ]
% otpcl> print $foo
% howdy
% ok
% '''
%
% In fact, most OTPCL features are in turn implemented as OTPCL-aware
% command-backing functions; that is: OTPCL exposes its own
% functionality as OTPCL commands wherever it's possible/practical to
% do so.
%
% Of course, one may also do this from any OTP application that uses
% OTPCL, e.g. one written in Erlang:
%
% ```
% erl> State0 = otpcl_env:default_state().
% [ ... pile of interpreter state ... ]
% erl> {ok, State1} = otpcl_stdlib:set([foo, <<"howdy~n">>], State0).
% [ ... pile of interpreter state ... ]
% erl> {ok, State2} = otpcl_eval:eval("print $foo", State1).
% howdy
% [ ... pile of interpreter state ... ]
% '''
-module(otpcl_eval).

-include("otpcl.hrl").

-export(['CMD_interpret'/2, interpret/1, interpret/2, 'CMD_eval'/2, eval/1,
         eval/2, 'CMD_eval_file'/2, eval_file/1, eval_file/2, make_charstring/1,
         make_binstring/1, make_atomic/1, make_atom/1]).

'CMD_interpret'([AST], State) ->
    interpret(AST, State);
'CMD_interpret'([AST, InnerState], OuterState) ->
    {interpret(AST, InnerState), OuterState}.

'CMD_eval'([Text], State) ->
    eval(Text, State);
'CMD_eval'([Text, InnerState], OuterState) ->
    {eval(Text, InnerState), OuterState}.

'CMD_eval_file'([Path], State) ->
    eval_file(Path, State);
'CMD_eval_file'([Path, InnerState], OuterState) ->
    {eval_file(Path, InnerState), OuterState}.

-ifdef(DEBUG).
-define(DEBUG_PRINT(Msg, Args), io:format(Msg, Args)).
-else.
-define(DEBUG_PRINT(Msg, Args), ok).
-endif.

% Build stuff out of tokens

-spec make_charstring([token()]) -> string().
% @doc Extract a character string from a token string.
make_charstring(Tokens) ->
    [C || {C,_} <- Tokens].

make_charstring(Tokens, _State) ->
    make_charstring(Tokens).

-spec make_binstring([token()]) -> binary().
% @doc Extract a binary string from a token string.
make_binstring(Tokens) ->
    list_to_binary(make_charstring(Tokens)).

make_binstring(Tokens, _State) ->
    make_binstring(Tokens).

-spec make_atomic([token()]) -> atom() | integer() | float().
% @doc Extract a float, integer, or atom (in order of preference) from
% a token string.
make_atomic(Tokens) ->
    make_atomic(Tokens, otpcl_env:minimal_state()).

make_atomic(Tokens, State) ->
    Text = make_charstring(Tokens),
    case interpreter_is_stringy(State) of
        true ->
            list_to_binary(Text);
        false ->
            make_atomic(Text, float, string:to_float(Text), State)
    end.

% Floats
make_atomic(_, float, {Float, []}, _State) ->
    Float;
make_atomic(Text, float, _, State) ->
    make_atomic(Text, integer, string:to_integer(Text), State);

% Integers (if this conversion attempt fails, then we just treat it as
% an ordinary atom)
make_atomic(_, integer, {Int, []}, _State) ->
    Int;
make_atomic(Text, integer, _, _State) ->
    list_to_atom(Text).

-spec make_atom([token()]) -> atom().
% @doc Extract an atom from a token string.  This skips any attempt to
% check if an atom is a number (which means single-quoted atoms might
% technically be more efficient than unquoted atoms at the moment...).
make_atom(Tokens) ->
    make_atom(Tokens, otpcl_env:minimal_state()).

make_atom(Tokens, State) ->
    Text = make_binstring(Tokens),
    case interpreter_is_stringy(State) of
        true ->
            Text;
        false ->
            binary_to_atom(Text)
    end.

% @doc Determines if the interpreter is "stringy" (i.e. it emits
% binstrings instead of atoms).
interpreter_is_stringy(State) ->
    case otpcl_meta:get(<<"STRINGY_INTERPRETER">>, State) of
        {error, _, _} ->
            false;
        _ ->
            true
    end.


% Here's the meat of the interpreter.

-spec interpret(tree() | [tree()]) -> eval_success() | eval_error().
% @doc Interpret the parse nodes with the default OTPCL starting state.
interpret(Nodes) ->
    interpret(Nodes, otpcl_env:default_state()).

-spec interpret(tree() | [tree()], state()) -> eval_success() | eval_error().
% @doc Interpret the parse nodes with a custom starting state.
interpret({parsed, unquoted, Tokens}, State) ->
    make_atomic(Tokens, State);
interpret({parsed, single_quoted, Tokens}, State) ->
    make_atom(Tokens, State);
interpret({parsed, double_quoted, Tokens}, _State) ->
    make_binstring(Tokens); % TODO: allow var/funcall substitution (maybe?)
interpret({parsed, braced, Tokens}, _State) ->
    make_binstring(Tokens);
interpret({parsed, backquoted, Tokens}, _State) ->
    make_charstring(Tokens);
interpret({parsed, var_unquoted, Tokens}, State) ->
    interpret({parsed, var, Tokens}, State);
interpret({parsed, var_braced, Tokens}, State) ->
    interpret({parsed, var, Tokens}, State);
interpret({parsed, var, Tokens}, State) ->
    {Val, State} = otpcl_meta:get(make_binstring(Tokens), State),
    Val;
% FIXME: any state changes here (new/modified functions and variables,
% for example) won't actually persist beyond a list/tuple/funcall
% literal until I define some better logic here.  This might end up
% being a "feature", though.
interpret({parsed, list, Items}, State) ->
    [interpret(I, State) || I <- Items];
interpret({parsed, tuple, Items}, State) ->
    list_to_tuple([interpret(I, State) || I <- Items]);
interpret({parsed, funcall, Words}, State) ->
    [Cmd|Args] = [interpret(I, State) || I <- Words],
    {Res, _} = otpcl_meta:apply(Cmd, Args, State),
    Res;
interpret({parsed, command, []}, State) ->
    otpcl_meta:get(<<"RETVAL">>, State);
interpret({parsed, command, Words}, State) ->
    [Cmd|Args] = [interpret(I, State) || I <- Words],
    otpcl_meta:apply(Cmd, Args, State);
interpret({parsed, comment, _}, State) ->
    otpcl_meta:get(<<"RETVAL">>, State);
interpret({parsed, program, [Cmd|Rest]}, State) ->
    {RetVal, NewState} = interpret(Cmd, State),
    {ok, RetState} = otpcl_meta:set(<<"RETVAL">>, RetVal, NewState),
    interpret({parsed, program, Rest}, RetState);
interpret({parsed, program, []}, State) ->
    otpcl_meta:get(<<"RETVAL">>, State);
interpret({parsed, Type, Data}, State) ->
    {error, {unknown_node_type, Type, Data}, State};
interpret(InvalidNode, State) ->
    {error, {not_an_otpcl_parse_node, InvalidNode}, State}.


% And some nice friendly wrappers around that interpreter

-spec eval(eval_input()) -> eval_success() | eval_error().
% @doc Evaluate a string with the default OTPCL starting state.
eval(Src) ->
    eval(Src, otpcl_env:default_state()).

-spec eval(eval_input(), state()) -> eval_success() | eval_error().
% @doc Evaluate a string with a custom starting state.
%% eval(Src = [Char|_], State) when is_integer(Char) ->
%%     eval([Src], State);
%% eval([Src, SubState], State) ->
%%     {eval([Src], SubState), State};
eval(Src, State) ->
    {ok, Tree, []} = otpcl_parse:parse(Src),
    interpret(Tree, State).
%% eval(Src, State) ->
%%     eval([Src], State).

-spec eval_file(filename()) -> eval_success() | eval_error().
% @doc Evaluate the named file with the default OTPCL starting state.
eval_file(Filename) ->
    eval_file(Filename, otpcl_env:default_state()).

-spec eval_file(filename(), state()) -> eval_success() | eval_error().
% @doc Evaluate the named file with a custom starting state.
eval_file(Filename = [Char|_], State) when is_integer(Char) ->
    eval_file([Filename], State);
eval_file([Filename], State) ->
    {ok, Src} = file:read_file(Filename),
    Tokens = otpcl_parse:scan(Src, otpcl_parse:initpos(Filename)),
    {ok, Tree, []} = otpcl_parse:parse(Tokens),
    interpret(Tree, State);
eval_file(Filename, State) ->
    eval_file([Filename], State).
