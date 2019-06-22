% @doc Entry point for the standalone OTPCL shell.  Currently only supports
% shell/REPL functionality, and notably does not include the ability to run
% OTPCL script files (this will be implemented Eventually™).
-module(otpcl_init).

-export([start/0, prompt/1]).

% @doc Start the standalone OTPCL shell.
start() ->
    %% Ripped the "dumb terminal" stuff from 'Elixir.IEx.CLI'.  I
    %% *thought* user_drv was doing this for me, but I think it's
    %% dropping the ball somewhere...
    case tty_works() of
        true ->
            user_drv:start('tty_sl -c -e', {otpcl_repl, start, []});
        _ ->
            application:set_env(stdlib, shell_prompt_func, {?MODULE, prompt}),
            user:start(),
            % I hope this is all I need to do for this; IEx does a lot
            % of fancy stuff here and I ain't exactly sure what's
            % actually relevant for my purposes.
            otpcl_repl:start()
    end.

% @hidden
prompt(_) ->
    [].

tty_works() ->
    try test_tty_sl() of
        _ -> true
    catch
        _:_ -> false
    end.

test_tty_sl() ->
    Port = open_port({spawn, 'tty_sl -c -e'}, [eof]),
    port_close(Port).
