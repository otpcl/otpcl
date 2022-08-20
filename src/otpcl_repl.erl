% @doc OTPCL shell/REPL.
-module(otpcl_repl).

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

-export([start/0, start/1, simple_shell/1, shell_init_state/1]).

% @doc Starts the OTPCL shell preloaded with commands useful for general
% shell-like usage (`ls', `cd', etc.).
start() ->
    start(shell_init_state(otpcl_env:default_state())).

% @doc Starts the OTPCL shell with a user-provided custom state.  Useful for
% implementing customized shells (e.g. restricted command shells for embedding
% in other OTP applications).
start(State) ->
    spawn(?MODULE, simple_shell, [State]).

% @doc A simple read-parse-eval loop.
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
        {error, {expected, _}, _, _, _} ->
            read(State, ps2(State), Input);
        {error, {unexpected, Char}, _, _, _} ->
            show_error(error, {unexpected, [Char]}),
            read(State, ps1(State), "");
        {error, Reason, _, _, _} ->
            show_error(error, Reason),
            read(State, ps1(State), "")
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
        ?CATCH(error, {badmatch,{error,{Rsn,Val},_}}) ->
            ?HANDLE(Rsn, Val);
        ?CATCH(Anything, Else) ->
            ?HANDLE(Anything, Else)
    end.

%% Prompts.

ps1(State) ->
    case otpcl_meta:get('PS1', State) of
        {Prompt, State} ->
            Prompt;
        _ ->
            "otpcl> "
    end.

ps2(State) ->
    case otpcl_meta:get('PS2', State) of
        {Prompt, State} ->
            Prompt;
        _ ->
            "  ...> "
    end.

%% Show stuff

show_banner(_) ->
    io:format("OTPCL Shell (WIP!)~n~n").

show_retval(RetVal) ->
    io:format("~p~n", [RetVal]).

show_error(Step, Reason) ->
    io:format("~p: ~p~n", [Step, Reason]).

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

% @doc Given an input state, returns an output state with pre-imported shell
% commands and sensible default prompts.
shell_init_state(State0) ->
    {_, State1} = otpcl_meta:import(otpcl_shell, State0),
    {_, State2} = otpcl_meta:set('PS1', "otpcl> ", State1),
    {_, State3} = otpcl_meta:set('PS2', "  ...> ", State2),
    State3.
