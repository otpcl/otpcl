-module(otpcl_init).

-export([start/0]).

start() ->
    %% TODO: allow passing files or script strings at some point
    user_drv:start('tty_sl -c -e', shell_spec()).

shell_spec() ->
    {otpcl_shell, start, []}.
