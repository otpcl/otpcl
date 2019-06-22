% @doc OTPCL environment helpers
-module(otpcl_env).

-include("otpcl.hrl").

-export([default_state/0, minimal_state/0]).

-spec default_state() -> state().
% @doc The default OTPCL interpreter state.  Includes all the basics required
% for an (approximately) self-sufficient OTPCL session (i.e. with the
% unrestricted ability to affect the world beyond the interpreter state).
default_state() ->
    State0 = core_state(),
    {ok, State1} = otpcl_meta:import([otpcl_control], State0),
    {ok, State2} = otpcl_meta:import([otpcl_meta], State1),
    {ok, State3} = otpcl_meta:import([otpcl_pipes], State2),
    State3.

-spec minimal_state() -> state().
% @doc A minimal OTPCL interpreter state.  Includes the absolute bare bones
% necessary for the interpreter to not freak out (namely: setting an initial
% value for `$RETVAL' in case the interpreter encounters a program with no
% commands).
minimal_state() ->
    State0 = {#{}, #{}},
    {ok, State1} = otpcl_meta:set(['RETVAL', ok], State0),
    State1.

-spec core_state() -> state().
% @doc A slightly-less-minimal OTPCL interpreter state.  Same as
% `minimal_state/0', but with the commands from `otpcl_core' pre-imported.
core_state() ->
    State0 = minimal_state(),
    {ok, State1} = otpcl_meta:import([otpcl_core], State0),
    State1.
