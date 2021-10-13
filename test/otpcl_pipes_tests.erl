-module(otpcl_pipes_tests).
-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

pipe_test() ->
    {ok, State} = otpcl:eval("import erlang"),
    {V1, _State} = otpcl:eval("return `howdy` | list_to_atom", State),
    ?assertEqual(howdy, V1),
    ok.

send_test() ->
    % Pinger = spawn(?MODULE, pinger, []),
    Pinger = spawn(fun () ->
                           receive
                               {Pid, ping} ->
                                   Pid ! pong
                           end
                   end),
    {_V1, S1} = otpcl:set(<<"pinger">>, Pinger, otpcl_env:default_state()),
    {_V2, S2} = otpcl:set(<<"self">>, self(), S1),
    {_V3, _S3} = otpcl:eval("return <$self ping> |! $pinger", S2),
    receive
        Response ->
            ?assertEqual(pong, Response)
    end,
    ok.

andalso_test() ->
    {V1, _S1} = otpcl:eval("return false |& return fail"),
    ?assertEqual(false, V1),
    {V2, _S2} = otpcl:eval("return true |& return pass"),
    ?assertEqual(pass, V2),
    ok.

orelse_test() ->
    {V1, _S1} = otpcl:eval("return pass || return fail"),
    ?assertEqual(pass, V1),
    {V2, _S2} = otpcl:eval("return false || return pass"),
    ?assertEqual(pass, V2),
    ok.

splat_test() ->
    {_V1, S1} = waldo_state(),
    {V2, _S2} = otpcl:eval("return (waldo foo bar waldo baz) |*", S1),
    ?assertEqual(2, V2),
    {V3, _S3} = otpcl:eval("return (foo bar waldo baz) |* waldo", S1),
    ?assertEqual(2, V3),
    ok.

insert_test() ->
    {_V1, S1} = waldo_state(),
    {V2, _S2} = otpcl:eval("return waldo |# 0 foo bar baz", S1),
    ?assertEqual(-1, V2),
    {V3, _S3} = otpcl:eval("return waldo |# 1 waldo foo bar baz", S1),
    ?assertEqual(0, V3),
    {V4, _S4} = otpcl:eval("return waldo |# 3 waldo foo bar baz", S1),
    ?assertEqual(2, V4),
    ok.

insert_splat_test() ->
    {_V1, S1} = waldo_state(),
    {V2, _S2} = otpcl:eval("return (bar waldo) |#* 2 waldo foo baz", S1),
    ?assertEqual(2, V2),
    ok.

waldo_state() ->
    otpcl:cmd(<<"waldo">>, fun waldo/2, otpcl_env:default_state()).

waldo(Args, State) ->
    waldo(Args, State, 0).

waldo([waldo|_], State, Acc) ->
    {Acc, State};
waldo([_|Rest], State, Acc) ->
    waldo(Rest, State, Acc + 1);
waldo([], State, _) ->
    {-1, State}.
