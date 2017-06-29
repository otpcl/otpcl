-module(ecl).

-behaviour(application).

-export([ start/2,
          stop/1 ]).

start(_Type, _Args) ->
    {error, not_implemented}.

stop(_State) ->
    ok.
