-module(otpcl_tests).
-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

hello_world_test() ->
    {ok, State} = otpcl:eval("set foo {Hello, world!}"),
    {Val, State} = otpcl:get(foo, State),
    ?assertEqual(Val, <<"Hello, world!">>),
    ok.
