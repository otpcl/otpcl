-module(otpcl_eval).

-export([get_var/2, set_var/3, get_fun/2, set_fun/3, call_fun/3,
         default_state/0, eval/1, eval/2]).

-ifdef(DEBUG).
-define(DEBUG_PRINT(Msg, Args), io:format(Msg, Args)).
-else.
-define(DEBUG_PRINT(Msg, Args), ok).
-endif.

% Environment helpers

get_var(Name, {Funs, Vars}) ->
    case maps:find(Name, Vars) of
        {ok, Val} ->
            {ok, Name, Val, {Funs, Vars}};
        _ ->
            % RFC: should we throw an error?  Or should we return a
            % default?  We'll throw an error for now.
            {error, no_such_var, {Funs, Vars}}
    end.
set_var(Name, Val, {Funs, Vars}) ->
    {ok, Name, Val, {Funs, maps:put(Name, Val, Vars)}}.

get_fun(Name, {Funs, Vars}) ->
    case maps:find(Name, Funs) of
        {ok, Fun} ->
            {ok, Name, Fun, {Funs, Vars}};
        _ ->
            {error, no_such_fun, {Funs, Vars}}
    end.
set_fun(Name, Fun, {Funs, Vars}) ->
    {ok, Name, Fun, {maps:put(Name, Fun, Funs), Vars}}.
call_fun(Name, Args, State) ->
    {ok, Name, Fun, State} = get_fun(Name, State),
    apply(Fun, [Args, State]).

default_state() ->
    {default_funs(), default_vars()}.
default_funs() ->
    %% Every OTPCL fun must accept a list of arguments and an input
    %% state, and must return a return value and an output state.
    %% Should I or someone else ever get around to allowing direct
    %% Erlang-native function calls, said calls will need to include
    %% the necessary wrapping logic to deal with the OTPCL interpreter
    %% state.
    #{ set  => fun ([Name, Val], State) ->
                       {ok, Name, Val, NewState} = set_var(Name, Val, State),
                       {ok, NewState}
               end,
       puts => fun (Lines, State) ->
                       [io:format(L) || L <- Lines],
                       {ok, State}
               end }.
default_vars() ->
    #{}.

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
    interpret(Nodes, default_state()).

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
    {ok, _, Val, _} = get_var(make_atom(Tokens), State),
    Val;
interpret({parsed, var_braced, Tokens}, State) ->
    {ok, _, Val, _} = get_var(make_atom(Tokens), State),
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
    {Res, _} = call_fun(Name, Args, State),
    Res;
interpret({parsed, command, Words}, State) ->
    [Name|Args] = [interpret(I, State) || I <- Words],
    call_fun(Name, Args, State);
interpret({parsed, comment, _}, State) ->
    {ok, State};
interpret({parsed, program, [Cmd|Rest]}, State) ->
    {_, NewState} = interpret(Cmd, State),
    interpret({parsed, program, Rest}, NewState);
interpret({parsed, program, []}, State) ->
    {ok, State};
interpret({parsed, Type, Data}, State) ->
    {error, unknown_node_type, Type, Data, State};
interpret(InvalidNode, State) ->
    {error, not_an_otpcl_parse_node, InvalidNode, State}.


% And a nice friendly wrapper around that interpreter

eval(Src) ->
    eval(Src, default_state()).

eval(Src, State) ->
    {ok, Nodes, []} = otpcl_parse:parse(Src),
    interpret(Nodes, State).
