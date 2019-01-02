-module(otpcl_eval).

-export([interpret/1, interpret/2, eval/1, eval/2, eval_file/1, eval_file/2]).

-ifdef(DEBUG).
-define(DEBUG_PRINT(Msg, Args), io:format(Msg, Args)).
-else.
-define(DEBUG_PRINT(Msg, Args), ok).
-endif.

% Build stuff out of tokens

make_charstring(Tokens) ->
    [C || {C,_} <- Tokens].

make_binstring(Tokens) ->
    list_to_binary(make_charstring(Tokens)).

% make_atomic/1 will default to creating an atom, but includes
% special-case logic for integers and floats.  There's probably a more
% efficient way of going about this than just trying the conversions
% and seeing if they've failed, but whatever.
make_atomic(Tokens) ->
    Text = make_charstring(Tokens),
    make_atomic(Text, float, string:to_float(Text)).

% Floats
make_atomic(_, float, {Float, []}) ->
    Float;
make_atomic(Text, float, _) ->
    make_atomic(Text, integer, string:to_integer(Text));

% Integers (if this conversion attempt fails, then we just treat it as
% an ordinary atom)
make_atomic(_, integer, {Int, []}) ->
    Int;
make_atomic(Text, integer, _) ->
    list_to_atom(Text).

% Shortcut straight to atom creation (e.g. if it's obvious that we
% don't need to attempt numeric conversion, e.g. for single-quoted
% atoms).
make_atom(Tokens) ->
    list_to_atom(make_charstring(Tokens)).


% Here's the meat of the interpreter.

interpret(Nodes) ->
    interpret(Nodes, otpcl_env:default_state()).

interpret({parsed, unquoted, Tokens}, _State) ->
    make_atomic(Tokens);
interpret({parsed, single_quoted, Tokens}, _State) ->
    make_atom(Tokens);
interpret({parsed, double_quoted, Tokens}, _State) ->
    make_binstring(Tokens); % TODO: allow var/funcall substitution
interpret({parsed, braced, Tokens}, _State) ->
    make_binstring(Tokens);
interpret({parsed, backquoted, Tokens}, _State) ->
    make_charstring(Tokens);
interpret({parsed, var_unquoted, Tokens}, State) ->
    {ok, _, Val, _} = otpcl_env:get_var(make_atom(Tokens), State),
    Val;
interpret({parsed, var_braced, Tokens}, State) ->
    {ok, _, Val, _} = otpcl_env:get_var(make_atom(Tokens), State),
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
    [Name|Args] = [interpret(I, State) || I <- Words],
    {Res, _} = otpcl_env:call_fun(Name, Args, State),
    Res;
interpret({parsed, command, Words}, State) ->
    [Name|Args] = [interpret(I, State) || I <- Words],
    otpcl_env:call_fun(Name, Args, State);
interpret({parsed, comment, _}, State) ->
    {ok, State};
interpret({parsed, program, [Cmd|Rest]}, State) ->
    {RetVal, NewState} = interpret(Cmd, State),
    {ok, 'RETVAL', RetVal, RetState} = otpcl_env:set_var('RETVAL', RetVal,
                                                         NewState),
    interpret({parsed, program, Rest}, RetState);
interpret({parsed, program, []}, State) ->
    {ok, 'RETVAL', RetVal, State} = otpcl_env:get_var('RETVAL', State),
    {RetVal, State};
interpret({parsed, Type, Data}, State) ->
    {error, unknown_node_type, Type, Data, State};
interpret(InvalidNode, State) ->
    {error, not_an_otpcl_parse_node, InvalidNode, State}.


% And some nice friendly wrappers around that interpreter

eval(Src) ->
    eval(Src, otpcl_env:default_state()).

eval(Src, State) ->
    {ok, Tree, []} = otpcl_parse:parse(Src),
    interpret(Tree, State).

eval_file(Filename) ->
    eval_file(Filename, otpcl_env:default_state()).

eval_file(Filename, State) ->
    {ok, Src} = file:read_file(Filename),
    Tokens = otpcl_parse:scan(Src, otpcl_parse:initpos(Filename)),
    {ok, Tree, []} = otpcl_parse:parse(Tokens),
    interpret(Tree, State).
