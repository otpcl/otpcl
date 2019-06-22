% @doc OTPCL meta-commands.  These commands all pertain to modifying the OTPCL
% interpreter's execution state/environment from within an OTPCL program; if you
% intend for your OTPCL-based DSL or what have you to be Turing-complete, this
% would be the module to either import or reimplement (and likewise, if you
% <em>don't</em> want your OTPCL-based DSL or what have you to be
% Turing-complete - e.g. for restricted/safe configuration files - this would be
% the module to exclude).
-module(otpcl_meta).

-include("otpcl.hrl").

-export([import/2, use/2, subcmd/2, cmd/2, apply/2, get/2, set/2]).

-otpcl_cmds([import, use, subcmd, cmd, apply, get, set]).

% @doc Get the value of the named variable.  Mostly useless from within OTPCL,
% but quite handy when manipulating OTPCL states from within Erlang or some
% other situation external to OTPCL.
get([Name], {Funs, Vars}) ->
    case maps:find(Name, Vars) of
        {ok, Val} ->
            {Val, {Funs, Vars}};
        _ ->
            {error, {no_such_var, Name}, {Funs, Vars}}
    end.

% @doc Set the value of the named variable.
set([Name, Val], {Funs, Vars}) ->
    {ok, {Funs, maps:put(Name, Val, Vars)}}.

% @doc Get (and optionally set) the named variable.  If setting a new value for
% an existing variable, this will return the old value (else, it'll return
% `ok').
var([Name], {Funs, Vars}) ->
    case maps:find(Name, Vars) of
        {ok, Val} ->
            {Val, {Funs, Vars}};
        _ ->
            {error, {no_such_var, Name}, {Funs, Vars}}
    end;
var([Name, Val], {Funs, Vars}) ->
    Search = maps:find(Name, Vars),
    NewVars = maps:put(Name, Val, Vars),
    case Search of
        {ok, Old} ->
            {Old, {Funs, NewVars}};
        _ ->
            {ok, {Funs, NewVars}}
    end.

-spec import([atom(), ...], state()) -> {'ok', state()}.
% @doc Imports commands from an Erlang module.  Will import either all commands
% (if only a module name is provided) or specifically-named commands (if any are
% passed after the module name).  If the module includes an `-otpcl_cmds'
% attribute with a list of command names (corresponding to 2-arity functions in
% that module), OTPCL will import these functions (and only these functions) as
% OTPCL commands outright (that is: it will assume that the module has
% such-named 2-arity functions exported/defined, and that those functions each
% accept a parameter list + state and return a tuple with a return value +
% state); else, OTPCL will "wrap" each imported function in a command that
% simply calls that function with the provided arguments and returns the result
% (without touching the input state).
%
% Either mode of operation can be forced by passing either `otpcl' or `erlang'
% (respectively) before the module name.  Note that `otpcl' is the default for
% importing a whole module, while `erlang' is the default for importing specific
% functions.  Also note that OTPCL doesn't really have a concept of "arity" (at
% least in the "`foo/1' and `foo/2' are different functions" sense), so if your
% module `foo' defines `bar/1' and `bar/2', `import foo bar' will create a `bar'
% command that wraps both.
%
% To summarize:
%
% ```
% import foo                # imports everything in module foo
% import foo bar baz        # imports bar and baz from foo
% import otpcl foo bar baz  # forcibly treats bar and baz as OTPCL commands
% import erlang foo bar baz # forcibly treats bar and baz as Erlang functions
% '''
%
% It's usually preferable to choose `use' over `import', since `use' avoids
% namespace clashes.  Both mechanisms are provided, though, for those who prefer
% brevity.
import([Module], State) ->
    import([Module, otpcl_cmds(Module:module_info(attributes))], State);
import([Module, {otpcl_cmds, Names}], State) ->
    import([otpcl, Module, Names], State);
import([Module, no_otpcl_cmds], State) ->
    Names = [Name || {Name, _} <- Module:module_info(exports)],
    import([erlang, Module, Names], State);
import([Type, Module, [Name|Names]], State) ->
    {ok, NewState} = import([Type, Module, Name], State),
    import([Type, Module, Names], NewState);
import([otpcl, _Module, []], State) ->
    {ok, State};
import([Module, Name], State) ->
    import([erlang, Module, Name], State);
import([otpcl, Module, Name], State) ->
    cmd([Name, fun Module:Name/2], State);
import([erlang, Module, Name], State) ->
    WrappedFun = fun (Args, S) -> {erlang:apply(Module, Name, Args), S} end,
    cmd([Name, WrappedFun], State).

otpcl_cmds([{otpcl_cmds, Names}|_]) ->
    {otpcl_cmds, Names};
otpcl_cmds([{_,_}|Rem]) ->
    otpcl_cmds(Rem);
otpcl_cmds([]) ->
    no_otpcl_cmds.

% @doc Creates a command representing a module.  The generated command will
% dispatch subcommands against the list of exported functions in the module
% (i.e. each function becomes a subcommand of the final command).  To summarize:
%
% ```
% use foo         # create command foo with foo's funs as subcommands
% use foo as bar  # create command bar with foo's funs as subcommands
% use otpcl foo   # forcibly treat all subcommand funs as OTPCL-aware
% use erlang foo  # forcibly treat all subcommand funs as non-OTPCL-aware
% '''
use([Module], State) ->
    use([Module, as, Module], State);
use([Module, as, Alias], State) ->
    use([Module, as, Alias, otpcl_cmds(Module:module_info(attributes))], State);
use([Module, as, Alias, {otpcl_cmds, Names}], State) ->
    use([otpcl, Module, as, Alias, Names], State);
use([Module, as, Alias, no_otpcl_cmds], State) ->
    Names = [Name || {Name, _} <- Module:module_info(exports)],
    use([erlang, Module, as, Alias, Names, []], State);
use([otpcl, Module, as, Alias, [Name|Names], Acc], State) ->
    use([otpcl, Module, as, Alias, Names, [Name, fun Module:Name/2|Acc]], State);
use([erlang, Module, as, Alias, [Name|Names], Acc], State) ->
    WrappedFun = fun (Args, S) -> {erlang:apply(Module, Name, Args), S} end,
    use([erlang, Module, as, Alias, Names, [Name, WrappedFun|Acc]], State);
use([_, _Module, as, Alias, [], Acc], State) ->
    {Dispatcher, NewState} = subcmd(Acc, State),
    cmd([Alias, Dispatcher], NewState).

% @doc Returns a subcommand dispatcher.  The resulting function (when set as a
% command) will treat the first argument as a subcommand name, look it up
% against an internal dictionary of subcommand names, and execute the
% corresponding subcommand function.
%
% Note that this is not strictly necessary in order to implement commands that
% use the subcommand pattern; OTPCL, like any good child of Erlang, will happily
% let you do the same thing ahead-of-time by allowing you to specify multiple
% argument specs and pattern match against the subcommand names.  `subcmd' is
% more intended for dynamic generation of subcommand dispatchers (e.g. for the
% `use' command in this very module).  There's certainly nothing stopping you
% from using `subcmd' instead of / in addition to ahead-of-time pattern
% matching, though.
subcmd(Args, State) ->
    subcmd(Args, State, #{}).

subcmd([Name, Body|SubCmds], State, Acc) ->
    subcmd(SubCmds, State, maps:put(Name, Body, Acc));
subcmd([], State, Acc) ->
    Dispatcher = fun ([Cmd|Args], SubState) ->
                         CmdFun = maps:get(Cmd, Acc),
                         ?MODULE:apply([CmdFun|Args], SubState)
                 end,
    {Dispatcher, State}.

% @doc Executes the specified command or function with the given arguments (if
% any).  If a function, assumes it's able to operate as an OTPCL command (that
% is: it's a 2-arity function that takes a list of arguments and a state).  If
% `pure' precedes the function, instead assumes that the function is meant to be
% an ordinary Erlang function and is "wrapped" (i.e. the input state and output
% state are identical, aside from a different `$RETVAL').
apply([pure, Fun | Args], State) when is_function(Fun) ->
    RetVal = erlang:apply(Fun, Args),
    {ok, NewState} = var(['$RETVAL', RetVal], State),
    {RetVal, NewState};
apply([Fun|Args], State) when is_function(Fun) ->
    erlang:apply(Fun, [Args, State]);
apply([Name|Args], State) ->
    case cmd([Name], State) of
        Err = {error, _, _} ->
            Err;
        {Fun, State} ->
            erlang:apply(Fun, [Args, State]);
        Err ->
            Err
    end.

% @doc Gets or sets the definition for the given command.  First argument is the
% command name.  Second argument is either a function (i.e. the kind produced
% via `fun' in Erlang or OTPCL; this function should be a 2-arity function
% accepting a list of arguments and a 2-element tuple of maps) or the argument
% list for the first of one or more pairs of argument lists and command bodies.
% Like Erlang functions, OTPCL commands support multiple definitions via pattern
% matching; unlike Erlang functions, they lack a concept of "arity", and also do
% not currently support guards (though this will hopefully be fixed in future
% versions of OTPCL).
%
% If no argument is passed to `cmd' after the command name, `cmd' will instead
% return the Erlang function backing that command.
cmd([Name], {Funs, Vars}) ->
    case maps:find(Name, Funs) of
        {ok, Fun} ->
            {Fun, {Funs, Vars}};
        _ ->
            {error, {no_such_fun, Name}, {Funs, Vars}}
    end;
cmd([Name, Fun], {Funs, Vars}) when is_function(Fun) ->
    {ok, {maps:put(Name, Fun, Funs), Vars}};
cmd(Args, State) ->
    cmd(Args, State, []).

% cmd's OTPCL-only mode is pretty complicated, since we basically have to write
% a mini-interpreter specifically for parsing the argspecs.

cmd([Name, <<"">>, Body | Clauses], State, Acc) ->
    cmd([Name, <<"# EMPTY">>, Body | Clauses], State, Acc);
cmd([Name, ArgSpec, Body | Clauses], State, Acc) ->
    {ok, {parsed, program, SubClauses}, []} = otpcl_parse:parse(ArgSpec),
    BuiltSCs = [build_subclause(SC, Body) || SC <- SubClauses],
    cmd([Name | Clauses], State, BuiltSCs ++ Acc);
cmd([Name], State, Acc) ->
    Clauses = lists:reverse(Acc),
    % FIXME: use a sane line number here instead of defaulting to line 0.
    Eval = {'fun', 0, {clauses, Clauses}},
    {value, Fun, _} = erl_eval:expr(Eval, []),
    cmd([Name, Fun], State).

build_subclause({parsed, comment, _}, Body) ->
    Args = erl_parse:abstract([]),
    {clause, 0, [Args, {var, 0, 'State0'}], [], build_body([], Body)};
build_subclause({parsed, command, Words}, Body) ->
    % FIXME: use a sane line number here instead of defaulting to line 0.
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
% Tuples and lists might have to-be-bound variables inside them, so we can't
% just shove 'em into erl_parse:abstract/1.
build_args([{parsed, tuple, Items}|Rem], Acc, Vars) ->
    {ItemArgs, ItemVars} = build_args(Items),
    build_args(Rem, [{tuple, 0, ItemArgs}|Acc], ItemVars ++ Vars);
% Of *course* Erlang can't just provide a simple {list,LINE,L} thing; it just
% *has* to be all lispy with its cons cells and such.
build_args([{parsed, list, Items}|Rem], Acc, Vars) ->
    build_args([{parsed, list, lists:reverse(Items), {nil,1}}|Rem], Acc, Vars);
build_args([{parsed, list, [I|Items], Conses}|Rem], Acc, Vars) ->
    {ConsInner, ConsVars} = build_args(I),
    Cons = {cons, 0, ConsInner, Conses},
    build_args([{parsed, list, Items, Cons}|Rem], Acc, ConsVars ++ Vars);
build_args([{parsed, list, [], Conses}|Rem], Acc, Vars) ->
    build_args(Rem, [Conses|Acc], Vars);
% TODO: do something cool with funcalls in argspecs instead of just ignoring
% them.  Maybe they could be used for guards?
build_args([{parsed, funcall, _}|Rem], Acc, Vars) ->
    build_args(Rem, Acc, Vars);
build_args([{parsed, comment, _}|Rem], Acc, Vars) ->
    build_args(Rem, Acc, Vars);
% All other word types should be safe to blindly interpret and shove into the
% Erlang parse tree we're building.
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
    Mod = {atom, 0, otpcl_eval},
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
    FunRef = {remote, 0, {atom, 0, otpcl_meta}, {atom, 0, set}},
    Args = [NameCons, StateVar],
    {call, 1, FunRef, Args}.

state_name(StateIdx) ->
    list_to_atom("State" ++ integer_to_list(StateIdx)).
