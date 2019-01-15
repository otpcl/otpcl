-module(otpcl_shell).

-if(?OTP_RELEASE >= 21).
-define(CATCH(Type, Reason), Type:Reason:Stacktrace).
-define(HANDLE(Type, Reason),
        show_error(Type, Reason, Stacktrace),
        read(State, ps1(State), "")).
-else.
-define(CATCH(Type, Reason), Type:Reason).
-define(HANDLE(Type, Reason),
        show_error(Type, Reason, erlang:get_stacktrace()),
        read(State, ps1(State), "")).
-endif.

-export([start/0, start/1, simple_shell/1]).

start() ->
    start(otpcl_env:default_state()).

start(State) ->
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
        ?CATCH(Anything, Else) ->
            ?HANDLE(Anything, Else)
    end.

eval(State, Tree) ->
    try otpcl_eval:interpret(Tree, State) of
        {RetVal, NewState} ->
            show_retval(RetVal),
            read(NewState, ps1(NewState), "")
    catch
        ?CATCH(error, {badmatch,{error,{Rsn,Val},State}}) ->
            ?HANDLE(Rsn, Val);
        ?CATCH(Anything, Else) ->
            ?HANDLE(Anything, Else)
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

show_error(Step, Reason, Stacktrace) ->
    io:format("~p: ~p~nStacktrace:~n", [Step, Reason]),
    show_trace(Stacktrace).

show_trace([{Mod,Fun,Arity,Locs}|Rem]) ->
    io:format("  ~p:~p/~p~n", [Mod,Fun,Arity]),
    show_trace_locs(Locs),
    show_trace(Rem);
show_trace([]) ->
    ok.

show_trace_locs([{Type,Val}|Rem]) ->
    io:format("    ~p: ~p~n", [Type,Val]),
    show_trace_locs(Rem);
show_trace_locs([]) ->
    ok.

