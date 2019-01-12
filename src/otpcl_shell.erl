-module(otpcl_shell).

-export([start/0, start/1, simple_shell/1]).

start() ->
    start(otpcl_env:default_state()).

start(State) ->
    %% spawn(fun () -> shell_init(State) end).
    spawn(?MODULE, simple_shell, [State]).

simple_shell(State) ->
    process_flag(trap_exit, true),
    show_banner(State),
    read(State, ps1(State), "").

read(State, Prompt, Acc) ->
    parse(State, Acc ++ io:get_line(Prompt)).

parse(State, Input) ->
    try otpcl_parse:parse(Input) of
        {ok, Tree, []} ->
            eval(State, Tree);
        {error, {expected, _}, _} ->
            read(State, ps2(State), Input)
    catch
        Anything:Else ->
            show_error(Anything, Else),
            read(State, ps1(State), "")
    end.

eval(State, Tree) ->
    try otpcl_eval:interpret(Tree, State) of
        {RetVal, NewState} ->
            show_retval(RetVal),
            read(NewState, ps1(NewState), "");
        AnythingElse ->
            show_error(eval, AnythingElse)
    catch
        Anything:Else ->
            show_error(Anything, Else),
            read(State, ps1(State), "")
    end.

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
    io:format("~p~n", [RetVal]).

show_error(Step, Reason) ->
    io:format("~p error: ~p~n", [Step, Reason]).
