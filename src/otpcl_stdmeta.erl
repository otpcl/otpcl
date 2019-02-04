-module(otpcl_stdmeta).

-export([import/2, eval/2, 'fun'/2, def/2]).

-otpcl_funs([import, eval, 'fun', def]).

%% import

% Import the whole module...
import([Module], State) ->
    import([Module, otpcl_funs(Module:module_info(attributes))], State);
% ...if it specifies Made For OTPCL™ functions
import([Module, {otpcl_funs, Names}], State) ->
    import([otpcl, Module, Names], State);
% ...if it doesn't
import([Module, no_otpcl_funs], State) ->
    Names = [Name || {Name, _} <- Module:module_info(exports)],
    import([erlang, Module, Names], State);
% Import a list of functions from a module
import([Type, Module, [Name|Names]], State) ->
    {ok, NewState} = import([Type, Module, Name], State),
    import([Type, Module, Names], NewState);
import([otpcl, Module, []], State) ->
    {{ok, Module}, State};
% Import a specific function...
import([Module, Name], State) ->
    import([erlang, Module, Name], State);
% ...if it's Made For OTPCL™
import([otpcl, Module, Name], State) ->
    'fun'([set, Name, fun Module:Name/2], State);
% ...if it's a normal Erlang function (the default assumption)
import([erlang, Module, Name], State) ->
    WrappedFun = fun (Args, S) -> {apply(Module, Name, Args), S} end,
    'fun'([set, Name, WrappedFun], State).

otpcl_funs([{otpcl_funs, Names}|_]) ->
    {otpcl_funs, Names};
otpcl_funs([{_,_}|Rem]) ->
    otpcl_funs(Rem);
otpcl_funs([]) ->
    no_otpcl_funs.


%% eval

eval([erlang, Txt], State) when is_binary(Txt) ->
    eval([erlang, binary_to_list(Txt)], State);
eval([erlang, Txt], State) ->
    {ok, Tokens, _} = erl_scan:string(Txt),
    {ok, Parsed} = erl_parse:parse_exprs(Tokens),
    {value, Result, _} = erl_eval:exprs(Parsed, []),
    {Result, State};
eval([Name, Args], State) ->
    {Fun, State} = 'fun'([get, Name], State),
    apply(Fun, [Args, State]);
eval([Txt], State) ->
    otpcl_eval:eval(Txt, State).


%% This is where the fun begins

'fun'([get, Name], {Funs, Vars}) ->
    case maps:find(Name, Funs) of
        {ok, Fun} ->
            {Fun, {Funs, Vars}};
        _ ->
            {error, {no_such_fun, Name}, {Funs, Vars}}
    end;

'fun'([set, Name, Fun], {Funs, Vars}) when is_function(Fun) ->
    {ok, {maps:put(Name, Fun, Funs), Vars}};

'fun'([call, Fun, Args], State) when is_function(Fun) ->
    apply(Fun, [Args, State]);
'fun'([call, Name, Args], State) ->
    case 'fun'([get, Name], State) of
        Err = {error, _, _} ->
            Err;
        {Fun, State} ->
            apply(Fun, [Args, State])
    end;

'fun'([wrap, Fun], State) when is_function(Fun) ->
    Wrapped = fun (Args, FunState) ->
                      {apply(Fun, Args), FunState}
              end,
    {Wrapped, State}.


% def is pretty complicated, since we basically have to write a
% mini-interpreter specifically for parsing the argspecs.

def(Args, State) ->
    def(Args, State, []).

% TODO: allow guards
def([Name, <<"">>, Body | Clauses], State, Acc) ->
    def([Name, <<"# EMPTY">>, Body | Clauses], State, Acc);
def([Name, ArgSpec, Body | Clauses], State, Acc) ->
    {ok, {parsed, program, SubClauses}, []} = otpcl_parse:parse(ArgSpec),
    BuiltSCs = [build_subclause(SC, Body) || SC <- SubClauses],
    def([Name | Clauses], State, BuiltSCs ++ Acc);
def([Name], State, Acc) ->
    Clauses = lists:reverse(Acc),
    % FIXME: use a sane line number here instead of defaulting to line
    % 0.
    Eval = {'fun', 0, {clauses, Clauses}},
    {value, Fun, _} = erl_eval:expr(Eval, []),
    'fun'([set, Name, Fun], State).

build_subclause({parsed, comment, _}, Body) ->
    Args = erl_parse:abstract([]),
    {clause, 0, [Args, {var, 0, 'State0'}], [], build_body([], Body)};
build_subclause({parsed, command, Words}, Body) ->
    % FIXME: use a sane line number here instead of defaulting to line
    % 0.
    {Args, Vars} = build_args(Words),
    {clause, 0, [Args, {var, 0, 'State0'}], [], build_body(Vars, Body)}.

build_args(Words) ->
    build_args(Words, [], []).

build_args([{parsed, var_unquoted, Tokens}|Rem], Acc, Vars) ->
    build_args([{parsed, var, Tokens}|Rem], Acc, Vars);
build_args([{parsed, var_braced, Tokens}|Rem], Acc, Vars) ->
    build_args([{parsed, var, Tokens}|Rem], Acc, Vars);
build_args([{parsed, var, Tokens}|Rem], Acc, Vars) ->
    Var = otpcl_eval:make_atom(Tokens),
    build_args(Rem, [{var, 0, Var}|Acc], [Var|Vars]);
% Tuples and lists might have to-be-bound variables inside them, so we
% can't just shove 'em into erl_parse:abstract/1.
build_args([{parsed, tuple, Items}|Rem], Acc, Vars) ->
    {ItemArgs, ItemVars} = build_args(Items),
    build_args(Rem, [{tuple, 0, ItemArgs}|Acc], ItemVars ++ Vars);
% Of *course* Erlang can't just provide a simple {list,LINE,L} thing;
% it just *has* to be all lispy with its cons cells and such.
build_args([{parsed, list, Items}|Rem], Acc, Vars) ->
    build_args([{parsed, list, lists:reverse(Items), {nil,1}}|Rem], Acc, Vars);
build_args([{parsed, list, [I|Items], Conses}|Rem], Acc, Vars) ->
    {ConsInner, ConsVars} = build_args(I),
    Cons = {cons, 0, ConsInner, Conses},
    build_args([{parsed, list, Items, Cons}|Rem], Acc, ConsVars ++ Vars);
build_args([{parsed, list, [], Conses}|Rem], Acc, Vars) ->
    build_args(Rem, [Conses|Acc], Vars);
% TODO: do something cool with funcalls in argspecs instead of just
% ignoring them.  Maybe they could be used for guards?
build_args([{parsed, funcall, _}|Rem], Acc, Vars) ->
    build_args(Rem, Acc, Vars);
build_args([{parsed, comment, _}|Rem], Acc, Vars) ->
    build_args(Rem, Acc, Vars);
% All other word types should be safe to blindly interpret and shove
% into the Erlang parse tree we're building.
build_args([Arg = {parsed, _, _}|Rem], Acc, Vars) ->
    Interpreted = otpcl_eval:interpret(Arg),
    Abstracted = erl_parse:abstract(Interpreted),
    build_args(Rem, [Abstracted|Acc], Vars);
% We're done
build_args([], Acc, Vars) ->
    {consify(Acc, {nil,0}), Vars};
% Catch-all for anything else.
build_args(Rem, Acc, Vars) ->
    {error, invalid_argspec, Rem, Acc, Vars}.

consify([Item|List], Conses) ->
    consify(List, {cons, 0, Item, Conses});
consify([], Conses) ->
    Conses.

build_body(Vars, Body) ->
    build_body(Vars, Body, [], 0).

build_body([Var|Vars], Body, BuiltVars, StateIdx) ->
    Left = build_match_left(StateIdx + 1),
    Right = build_match_right(Var, StateIdx),
    Match = {match, 0, Left, Right},
    build_body(Vars, Body, [Match|BuiltVars], StateIdx + 1);
build_body([], Body, BuiltVars, StateIdx) ->
    Wrapped = erl_parse:abstract([Body]),
    StateN = {var, 0, state_name(StateIdx)},
    Mod = {atom, 0, otpcl_stdmeta},
    Fun = {atom, 0, eval},
    Call = {call, 1, {remote, 1, Mod, Fun}, [Wrapped, StateN]},
    lists:reverse([Call|BuiltVars]).

build_match_left(StateIdx) ->
    State = {var, 0, state_name(StateIdx)},
    Empty = {var, 0, '_'},
    {tuple, 0, [Empty, State]}.

build_match_right(VarName, StateIdx) ->
    StateName = state_name(StateIdx),
    ValCons = {cons, 0, {var, 0, VarName}, {nil, 0}},
    NameCons = {cons, 0, {atom, 0, VarName}, ValCons},
    StateVar = {var, 0, StateName},
    FunRef = {remote, 0, {atom, 0, otpcl_stdlib}, {atom, 0, set}},
    Args = [NameCons, StateVar],
    {call, 1, FunRef, Args}.

state_name(StateIdx) ->
    list_to_atom("State" ++ integer_to_list(StateIdx)).
