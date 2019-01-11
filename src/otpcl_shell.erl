-module(otpcl_shell).

-export([start/0, start/1, shell_init/1, parser_init/1, interpreter_init/1]).

start() ->
    start(otpcl_env:default_state()).

start(State) ->
    spawn(?MODULE, shell_init, [State]).

%% Shell process.  Handles the "P", "L", and half of the "R" in
%% "REPL"; sends messages to a parser process for other other half of
%% the "R", and either re-reads (potentially for more input if the
%% parser demands it) or sends the parse result to the interpreter
%% (the "E") and returns whatever the interpreter sends back,
%% successful or otherwise.

shell_init(State) ->
    process_flag(trap_exit, true),
    show_banner(State),
    Prompt = ps1(State),
    Procs = {start_parser(), start_interpreter()},
    shell_read(Prompt, Procs, State, "").

shell_read(Prompt, Procs = {P,I}, State, Input) ->
    P ! {parse, self(), io:read_line(Prompt)},
    receive
        {ok, P, Tree} ->
            shell_eval(Prompt, Procs, State, Tree);
        {moar, P, _} ->
            shell_read(ps2(State), Procs, State, Input);
        {'EXIT', P, Reason} ->
            show_error(parse, Reason),
            NewP = start_parser(),
            shell_read(Prompt, {NewP,I}, State, "")
    end.

shell_eval(_, Procs = {P,I}, State, Tree) ->
    I ! {interpret, self(), Tree, State},
    receive
        {ok, I, RetVal, NewState} ->
            show_retval(RetVal),
            shell_read(ps1(State), Procs, NewState, "");
        {'EXIT', I, Reason} ->
            show_error(eval, Reason),
            NewI = start_interpreter(),
            shell_read(ps1(State), {P,NewI}, State, "")
    end.

%% Parser process.  The back half of the "R" in "REPL".  It takes in
%% input text and tries to parse it.  If it fails to parse because of
%% a character that was expected but not found before EOF, it'll
%% return a special error message instead of dying.

start_parser() ->
    spawn_link(?MODULE, parser_init, [self()]).

parser_init(Shell) ->
    parser_listen(Shell).

parser_listen(Shell) ->
    receive
        {parse, Shell, Input} ->
            parser_reply(Shell, otpcl_parse:parse(Input))
    end.

parser_reply(Shell, {error, {expected, Char}, _}) ->
    Shell ! {moar, self(), Char},
    parser_listen(Shell);
parser_reply(Shell, {ok, Tree, _}) ->
    Shell ! {ok, self(), Tree},
    parser_listen(Shell).

%% Interpreter process.  The "E" in "REPL".  It takes an OTPCL parse
%% tree and state and tries to run it.

start_interpreter() ->
    spawn_link(?MODULE, interpreter_init, [self()]).

interpreter_init(Shell) ->
    interpreter_listen(Shell).

interpreter_listen(Shell) ->
    receive
        {interpret, Shell, Tree, State} ->
            interpreter_reply(Shell, otpcl_eval:interpret(Tree, State))
    end.

interpreter_reply(Shell, {RetVal, NewState}) ->
    Shell ! {ok, self(), RetVal, NewState},
    interpreter_listen(Shell).

%% Prompts.  TODO/FIXME: actually read the PS1 and PS2 variables from
%% the provided state (which is currently ignored).

ps1(_) ->
    "otpcl> ".

ps2(_) ->
    "  ...> ".

%% Show stuff

show_banner(_) ->
    io:format("OTPCL Shell (WIP!)~n~n").

show_retval(RetVal) ->
    io:format("~p", RetVal).

show_error(Step, Reason) ->
    io:format("~p error: ~p", [Step, Reason]).
