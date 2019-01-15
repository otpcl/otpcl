-module(otpcl_env).

-export([default_state/0]).

default_state() ->
    State0 = {#{}, #{}},
    {{ok, _}, State1} = otpcl_stdmeta:import([otpcl_stdlib], State0),
    {{ok, _}, State2} = otpcl_stdmeta:import([otpcl_stdmeta], State1),
    {ok, State3} = otpcl_stdlib:set(['RETVAL', ok], State2),
    State3.
