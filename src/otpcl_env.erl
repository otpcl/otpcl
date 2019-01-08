-module(otpcl_env).

-export([default_state/0]).

default_state() ->
    State0 = {#{}, #{}},
    {{ok, _}, State1} = otpcl_stdlib:import([otpcl_stdlib], State0),
    {ok, State2} = otpcl_stdlib:set(['RETVAL', ok], State1),
    State2.
