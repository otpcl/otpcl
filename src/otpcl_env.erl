-module(otpcl_env).

-export([get_var/2, set_var/3, get_fun/2, set_fun/3, call_fun/3, import_fun/3,
         default_state/0, default_funs/0, default_vars/0]).

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
import_fun(Module, Name, State) ->
    WrappedFun = fun (Args, S) -> {apply(Module, Name, Args), S} end,
    set_fun(Name, WrappedFun, State).

default_state() ->
    {default_funs(), default_vars()}.
default_funs() ->
    %% Every OTPCL fun must accept a list of arguments and an input
    %% state, and must return a return value and an output state.
    #{ set    => fun otpcl_stdlib:set/2,
       print  => fun otpcl_stdlib:print/2,
       'if'   => fun otpcl_stdlib:'if'/2,
       unless => fun otpcl_stdlib:unless/2,
       incr   => fun otpcl_stdlib:incr/2,
       decr   => fun otpcl_stdlib:decr/2,
       import => fun otpcl_stdlib:import/2,
       eval   => fun otpcl_stdlib:eval/2 }.
default_vars() ->
    #{'RETVAL' => ok}.
