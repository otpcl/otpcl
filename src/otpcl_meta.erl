% @doc OTPCL meta-commands.
%
% These commands all pertain to modifying the OTPCL interpreter's
% execution state/environment from within an OTPCL program; if you
% intend for your OTPCL-based DSL or what have you to be
% Turing-complete, this would be the module to either import or
% reimplement (and likewise, if you <em>don't</em> want your
% OTPCL-based DSL or what have you to be Turing-complete - e.g. for
% restricted/safe configuration files - this would be the module to
% exclude).
-module(otpcl_meta).

-include("otpcl.hrl").

-export(['CMD_import'/2, import/2, import/3, 'CMD_use'/2, use/2, use/3,
         'CMD_subcmd'/2, subcmd/2, 'CMD_cmd'/2, cmd/2, cmd/3, 'CMD_apply'/2,
         apply/3, 'CMD_get'/2, get/2, 'CMD_set'/2, set/3, 'CMD_unset'/2,
         unset/2, 'CMD_var'/2, var/2, var/3]).

% Wrappers

import(Name, State) ->
    'CMD_import'([Name], State).
import(Name, Exports, State) ->
    'CMD_import'([Name|Exports], State).
use(Name, State) ->
    'CMD_use'([Name], State).
use(Name, Alias, State) ->
    'CMD_use'([Name, <<"as">>, Alias], State).
subcmd(Args, State) ->
    'CMD_subcmd'(Args, State).
cmd(Name, State) ->
    'CMD_cmd'([Name], State).
cmd(Name, Body, State) when is_function(Body) ->
    'CMD_cmd'([Name, Body], State);
cmd(Name, Clauses, State) when is_list(Clauses) ->
    'CMD_cmd'([Name|Clauses], State).
apply(Fun, Args, State) ->
    'CMD_apply'([Fun|Args], State).
get(Name, State) ->
    'CMD_get'([Name], State).
set(Name, Value, State) ->
    'CMD_set'([Name, Value], State).
unset(Name, State) ->
    'CMD_unset'([Name], State).
var(Name, State) ->
    'CMD_var'([Name], State).
var(Name, Value, State) ->
    'CMD_var'([Name, Value], State).

make_binstring(I) when is_atom(I) ->
    atom_to_binary(I);
make_binstring(I) when is_list(I) ->
    list_to_binary(I);
make_binstring(I) when is_integer(I) ->
    integer_to_binary(I);
make_binstring(I) when is_float(I) ->
    float_to_binary(I);
make_binstring(I) when is_binary(I) ->
    I.

make_atom(I) when is_list(I) ->
    list_to_atom(I);
make_atom(I) when is_binary(I) ->
    binary_to_atom(I);
make_atom(I) when is_atom(I) ->
    I.

make_existing_atom(I) when is_list(I) ->
    list_to_existing_atom(I);
make_existing_atom(I) when is_binary(I) ->
    binary_to_existing_atom(I);
make_existing_atom(I) when is_atom(I) ->
    I.


% @doc Get the value of the named variable.
%
% Mostly useless from within OTPCL, but quite handy when manipulating
% OTPCL states from within Erlang or some other situation external to
% OTPCL.
'CMD_get'([N], {Funs, Vars}) ->
    Name = make_binstring(N),
    case maps:find(Name, Vars) of
        {ok, Val} ->
            {Val, {Funs, Vars}};
        _ ->
            {error, {no_such_var, Name}, {Funs, Vars}}
    end.


% @doc Set the value of the named variable.
'CMD_set'([N, Val], {Funs, Vars}) ->
    Name = make_binstring(N),
    {ok, {Funs, maps:put(Name, Val, Vars)}}.

% @doc Unset the named variable, as if it never existed.
'CMD_unset'([N], {Funs, Vars}) ->
    Name = make_binstring(N),
    {ok, {Funs, maps:remove(Name, Vars)}}.

% @doc Get (and optionally set) the named variable.
%
% If setting a new value for an existing variable, this will return
% the old value (else, it'll return `ok').
'CMD_var'([N], {Funs, Vars})->
    Name = make_binstring(N),
    case maps:find(Name, Vars) of
        {ok, Val} ->
            {Val, {Funs, Vars}};
        _ ->
            {error, {no_such_var, Name}, {Funs, Vars}}
    end;
'CMD_var'([N, Val], {Funs, Vars}) ->
    Name = make_binstring(N),
    Search = maps:find(Name, Vars),
    NewVars = maps:put(Name, Val, Vars),
    case Search of
        {ok, Old} ->
            {Old, {Funs, NewVars}};
        _ ->
            {ok, {Funs, NewVars}}
    end.

-spec import([atom(), ...], state()) -> {'ok', state()}.
% @doc Imports commands from an Erlang module.
%
% Will import either all commands (if only a module name is provided)
% or specifically-named commands (if any are passed after the module
% name).  If an exported function's name starts with `CMD_', `import'
% will treat that function as a proper state-altering OTPCL command
% named after whatever follows that prefix (e.g. `CMD_foo' becomes
% `foo').  Otherwise, `import' will treat that function as an ordinary
% Erlang function, creating an OTPCL command with the same name.
%
% To summarize:
%
% ```
% import foo                # imports everything in module foo
% import foo bar baz        # imports only bar and baz from foo
% '''
%
% It's usually preferable to choose `use' over `import', since `use'
% avoids namespace clashes.  Both mechanisms are provided, though, for
% those who prefer brevity.
%
% (<strong>Warning for "stringy" interpreter users:</strong> this
% command dynamically creates atoms!)
'CMD_import'([M], State) ->
    Module = make_existing_atom(M),
    Exported = exported_cmds(Module),
    Deduped = deduped_cmds(Exported),
    do_import(Deduped, State);
'CMD_import'([M|Names], State) ->
    Module = make_existing_atom(M),
    Exported = exported_cmds(Module),
    Deduped = deduped_cmds(Exported),
    Filtered = filtered_cmds(Deduped, Names),
    do_import(Filtered, State).

do_import([{Name, Fun}|Cmds], State) ->
    {_, NewState} = 'CMD_cmd'([Name, Fun], State),
    do_import(Cmds, NewState);
do_import([], State) ->
    {ok, State}.

% @doc Creates a command representing a module.
%
% The generated command will dispatch subcommands against the list of
% exported functions in the module (i.e. each function becomes a
% subcommand of the final command).  To summarize:
%
% ```
% use foo         # create command foo with foo's funs as subcommands
% use foo as bar  # create command bar with foo's funs as subcommands
% '''
%
% (<strong>Warning for "stringy" interpreter users:</strong> this
% command dynamically creates atoms!)
'CMD_use'([M], State) ->
    do_use(M, M, State);
'CMD_use'([M, as, A], State) ->
    do_use(M, A, State);
'CMD_use'([M, <<"as">>, A], State) ->
    do_use(M, A, State).

do_use(M, A, State) ->
    Module = make_existing_atom(M), Alias = make_existing_atom(A),
    Exported = exported_cmds(Module),
    Deduped = deduped_cmds(Exported),
    Prepped = prepped_cmds(Deduped, []),
    {Dispatcher, NewState} = 'CMD_subcmd'(Prepped, State),
    'CMD_cmd'([Alias, Dispatcher], NewState).

exported_cmds(Module) ->
    Names = [atom_to_list(Name) || {Name, _} <- Module:module_info(exports)],
    exported_cmds(Module, Names, []).

exported_cmds(Module, [Name|Names], Cmds) ->
    Cmd = fun2cmd(Module, Name),
    exported_cmds(Module, Names, [Cmd|Cmds]);
exported_cmds(_, [], Cmds) ->
    Cmds.

fun2cmd(Module, "CMD_" ++ CmdName = Name) ->
    AName = list_to_atom(Name),
    WrappedFun = fun Module:AName/2,
    {otpcl, CmdName, WrappedFun};
fun2cmd(Module, Name) ->
    AName = list_to_atom(Name),
    WrappedFun = fun (Args, State) ->
                         {erlang:apply(Module, AName, Args), State}
                 end,
    {erlang, Name, WrappedFun}.

deduped_cmds(Cmds) ->
    % FIXME: There's probably a much better way to do this.
    Partitioner = fun ({Kind,_,_}) ->
                          Kind == otpcl
                  end,
    {OtpclCmds, ErlangCmds} = lists:partition(Partitioner, Cmds),
    OtpclNames = [Name || {_, Name, _} <- OtpclCmds],
    % If there's an OTPCL command and an Erlang function with the same
    % command name, the former takes precedence - e.g. if the module
    % exports 'CMD_foo/2' and 'foo/1', then 'CMD_foo/2' will become
    % the 'foo' command provided to the output state from import/use.
    Keeper = fun ({erlang, Name, _}) ->
                      not lists:member(Name, OtpclNames)
             end,
    KeptErlangCmds = lists:filter(Keeper, ErlangCmds),
    DedupedCmds = OtpclCmds ++ KeptErlangCmds,
    [{Name, Fun} || {_, Name, Fun} <- DedupedCmds].

filtered_cmds(Cmds, Names) ->
    Taker = fun ({Name, _}) -> lists:member(Name, Names) end,
    lists:takewhile(Taker, Cmds).

prepped_cmds([{Name, Fun}|Cmds], Prepped) ->
    prepped_cmds(Cmds, [Name, Fun|Prepped]);
prepped_cmds([], Prepped) ->
    Prepped.

% @doc Returns a subcommand dispatcher.  The resulting function (when
% set as a command) will treat the first argument as a subcommand
% name, look it up against an internal dictionary of subcommand names,
% and execute the corresponding subcommand function.
%
% Note that this is not strictly necessary in order to implement
% commands that use the subcommand pattern; OTPCL, like any good child
% of Erlang, will happily let you do the same thing ahead-of-time by
% allowing you to specify multiple argument specs and pattern match
% against the subcommand names.  `subcmd' is more intended for dynamic
% generation of subcommand dispatchers (e.g. for the `use' command in
% this very module).  There's certainly nothing stopping you from
% using `subcmd' instead of / in addition to ahead-of-time pattern
% matching, though.
'CMD_subcmd'(Args, State) ->
    do_subcmd(Args, State, #{}).

do_subcmd([N, Body|SubCmds], State, Acc) when is_function(Body) ->
    Name = make_binstring(N),
    do_subcmd(SubCmds, State, maps:put(Name, Body, Acc));
do_subcmd([], State, Acc) ->
    Dispatcher = fun ([C|Args], SubState) ->
                         Cmd = make_binstring(C),
                         CmdFun = maps:get(Cmd, Acc),
                         ?MODULE:apply(CmdFun, Args, SubState)
                 end,
    {Dispatcher, State}.

% @doc Executes the specified command or function with the given
% arguments (if any).
%
% If a function, assumes it's able to operate as an OTPCL command
% (that is: it's a 2-arity function that takes a list of arguments and
% a state).  If `pure' precedes the function, instead assumes that the
% function is meant to be an ordinary Erlang function and is "wrapped"
% (i.e. the input state and output state are identical, aside from a
% different `$RETVAL').
'CMD_apply'([pure, Fun | Args], State) when is_function(Fun) ->
    RetVal = erlang:apply(Fun, Args),
    {ok, NewState} = 'CMD_var'([<<"RETVAL">>, RetVal], State),
    {RetVal, NewState};
'CMD_apply'([Fun|Args], State) when is_function(Fun) ->
    erlang:apply(Fun, [Args, State]);
'CMD_apply'([N|Args], State) ->
    Name = make_binstring(N),
    case 'CMD_cmd'([Name], State) of
        Err = {error, _, _} ->
            Err;
        {Fun, State} ->
            erlang:apply(Fun, [Args, State]);
        Err ->
            Err
    end.

% @doc Gets or sets the definition for the given command.
%
% First argument is the command name.  Second argument is either a
% function (i.e. the kind produced via `fun' in Erlang or OTPCL; this
% function should be a 2-arity function accepting a list of arguments
% and an OTPCL state) or the argument list for the first of one or
% more pairs of argument lists and command bodies.  Like Erlang
% functions, OTPCL commands support multiple definitions via pattern
% matching; unlike Erlang functions, they lack a concept of "arity",
% and also do not currently support guards (though this will hopefully
% be fixed in future versions of OTPCL).
%
% If no argument is passed to `cmd' after the command name, `cmd' will
% instead return the Erlang function backing that command.
%
% (<strong>Warning for "stringy" interpreter users:</strong> this
% command dynamically creates atoms!)
'CMD_cmd'([N], {Funs, Vars}) ->
    Name = make_binstring(N),
    case maps:find(Name, Funs) of
        {ok, Fun} ->
            {Fun, {Funs, Vars}};
        _ ->
            {error, {no_such_fun, Name}, {Funs, Vars}}
    end;
'CMD_cmd'([N, Fun], {Funs, Vars}) when is_function(Fun) ->
    Name = make_binstring(N),
    {ok, {maps:put(Name, Fun, Funs), Vars}};
'CMD_cmd'(Args, State) ->
    do_cmd(Args, State, []).

% cmd's OTPCL-only mode is pretty complicated, since we basically have
% to write a mini-interpreter specifically for parsing the argspecs.

do_cmd([N, <<"">>, Body | Clauses], State, Acc) ->
    Name = make_binstring(N),
    do_cmd([Name, <<"# EMPTY">>, Body | Clauses], State, Acc);
do_cmd([N, ArgSpec, Body | Clauses], State, Acc) ->
    Name = make_binstring(N),
    {ok, {parsed, program, SubClauses}, []} = otpcl_parse:parse(ArgSpec),
    BuiltSCs = [build_subclause(SC, Body) || SC <- SubClauses],
    do_cmd([Name | Clauses], State, BuiltSCs ++ Acc);
do_cmd([N], State, Acc) ->
    Name = make_binstring(N),
    Clauses = lists:reverse(Acc),
    % FIXME: use a sane line number here instead of defaulting to line 0.
    Eval = {'fun', 0, {clauses, Clauses}},
    {value, Fun, _} = erl_eval:expr(Eval, []),
    'CMD_cmd'([Name, Fun], State).

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
