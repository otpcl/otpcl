% @doc OTPCL environment helpers
-module(otpcl_env).

-include("otpcl.hrl").

-export([default_state/0, minimal_state/0, core_state/0, stringy_state/0]).

-spec default_state() -> state().
% @doc The default OTPCL interpreter state.
%
% Includes all the basics required for an (approximately)
% self-sufficient OTPCL session (i.e. with the unrestricted ability to
% affect the world beyond the interpreter state).
default_state() ->
    State0 = core_state(),
    {ok, State1} = otpcl_meta:import([otpcl_control], State0),
    {ok, State2} = otpcl_meta:import([otpcl_meta], State1),
    {ok, State3} = otpcl_meta:import([otpcl_pipes], State2),
    State3.

-spec minimal_state() -> state().
% @doc A minimal OTPCL interpreter state.
%
% Includes the absolute bare bones necessary for the interpreter to
% not freak out (namely: setting an initial value for `$RETVAL' in
% case the interpreter encounters a program with no commands).
minimal_state() ->
    State0 = {#{}, #{}},
    {ok, State1} = otpcl_meta:set([<<"RETVAL">>, ok], State0),
    State1.

-spec core_state() -> state().
% @doc A slightly-less-minimal OTPCL interpreter state.
%
% Same as `minimal_state/0', but with the commands from `otpcl_core'
% pre-imported, such that `return' and `|' (i.e. the basic pipe) are
% available.
%
% This is the recommended starting point for moderate-safety
% interpreters; it's as safe as it gets if you want to be able to
% supply Erlang functions to the interpreter without having to write
% extra wrappers, but it also defaults to being able to create
% arbitrary atoms, which risks exhausting Erlang's atom supply with
% sufficiently numerous/large/complex OTPCL scripts.
core_state() ->
    State0 = minimal_state(),
    {ok, State1} = otpcl_meta:import([otpcl_core], State0),
    State1.

-spec stringy_state() -> state().
% @doc A minimal OTPCL interpreter state, designed for "stringy"
% interpreters.
%
% That is: this state sets a special variable (`$STRINGY_INTERPRETER')
% which tells the OTPCL interpreter to generate (binary) strings
% whenever it would otherwise dynamically generate an atom or number.
% This is handy for situations where you want to be able to interpret
% potentially-adversarial OTPCL code (e.g. entered by users into a
% web-facing application), since it prevents users from (intentionally
% or otherwise) blowing out the Erlang runtime's limit on atoms.  It's
% also handy in general if you don't want OTPCL to be "clever" about
% argument types, e.g. to reduce having to check for whether a version
% number is an atom or float (e.g. 1.2 v. 1.2.0).
%
% Note that any commands invoked while the interpreter is "stringy"
% will receive strings instead of atoms/integers/floats as arguments;
% the "stringy" interpreter deliberately makes zero attempt at
% automatically converting arguments.  OTPCL's builtin commands
% generally try to be as forgiving as possible when it comes to
% argument types (converting as necessary), but all bets are off if
% you're calling Erlang functions as commands, or if you're calling
% OTPCL commands written by someone else.  It's very likely you'll
% need to write your own wrappers for such functions rather than
% relying on OTPCL's normal `import'/`use' commands.
%
% Note also that all bets are off within commands/functions
% themselves; just because the interpreter itself won't dynamically
% create atoms doesn't mean the commands you're calling will extend
% the same courtesy.  For this reason, the "stringy" interpreter state
% is deliberately barebones (only `return' and `|'), and it's up to
% you to validate the safety of whatever commands you import into this
% state.
%
% This is the recommended starting point for maximum-safety
% interpreters; by default, scripts using this state are unable to
% "escape" the state, and are unable to exhaust Erlang's atom supply
% (if either of these prove possible from exactly this state, then
% that is a Severity 0 bug, and you are strongly encouraged to report
% it to us immediately).
stringy_state() ->
    State0 = core_state(),
    {ok, State1} = otpcl_meta:set([<<"STRINGY_INTERPRETER">>, ok], State0),
    State1.
