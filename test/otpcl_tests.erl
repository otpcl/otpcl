-module(otpcl_tests).
-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

hello_world_test() ->
    {ok, State} = otpcl:eval("set foo {Hello, world!}"),
    {ok, foo, Val, State} = otpcl_env:get_var(foo, State),
    ?assertEqual(Val, <<"Hello, world!">>),
    ok.

