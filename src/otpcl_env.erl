% @doc OTPCL environment helpers
-module(otpcl_env).

-include("otpcl.hrl").

-export([default_state/0, minimal_state/0, core_state/0, stringy_state/0]).

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
    {ok, State1} = otpcl_meta:set([<<"RETVAL">>, ok], State0),
    State1.

-spec core_state() -> state().
% @doc A slightly-less-minimal OTPCL interpreter state.  Same as
% `minimal_state/0', but with the commands from `otpcl_core' pre-imported.
core_state() ->
    State0 = minimal_state(),
    {ok, State1} = otpcl_meta:import([otpcl_core], State0),
    State1.

-spec stringy_state() -> state().
% @doc A minimal OTPCL interpreter state, designed for "stringy"
% interpreters.  That is: this state sets a special variable
% (`$STRINGY_INTERPRETER') which tells the OTPCL interpreter to
% generate (binary) strings whenever it would otherwise dynamically
% generate an atom.  This is handy for situations where you want to be
% able to interpret potentially-adversarial OTPCL code (e.g. entered
% by users into a web-facing application), since it prevents users
% from (intentionally or otherwise) blowing out the Erlang runtime's
% limit on atoms.
%
% Note that any commands invoked within such an interpreter will need
% to be able to handle non-atom arguments.  OTPCL's builtin commands
% generally try to do this already as necessary, but all bets are off
% if you're calling Erlang functions as commands, or if you're calling
% OTPCL commands written by someone else.  It's very likely you'll
% need to write your own wrappers for such functions rather than
% relying on OTPCL's normal `import'/`use' commands.
%
% Note also that all bets are off within commands/functions
% themselves; just because the interpreter itself won't dynamically
% create atoms doesn't mean the commands you're calling will extend
% the same courtesy.  For this reason, the "stringy" interpreter state
% is deliberately barebones, and it's up to you to validate the safety
% of whatever commands you import into this state.
stringy_state() ->
    State0 = core_state(),
    {ok, State1} = otpcl_meta:set([<<"STRINGY_INTERPRETER">>, ok], State0),
    State1.
