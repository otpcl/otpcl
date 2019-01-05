-module(otpcl_env).

-export([default_state/0, default_funs/0, default_vars/0]).

default_state() ->
    {default_funs(), default_vars()}.
default_funs() ->
    %% Every OTPCL fun must accept a list of arguments and an input
    %% state, and must return a return value and an output state.
    #{ get    => fun otpcl_stdlib:get/2,
       set    => fun otpcl_stdlib:set/2,
       print  => fun otpcl_stdlib:print/2,
       'if'   => fun otpcl_stdlib:'if'/2,
       unless => fun otpcl_stdlib:unless/2,
       incr   => fun otpcl_stdlib:incr/2,
       decr   => fun otpcl_stdlib:decr/2,
       import => fun otpcl_stdlib:import/2,
       eval   => fun otpcl_stdlib:eval/2 }.
default_vars() ->
    #{'RETVAL' => ok}.
