-module(otpcl_tests).
-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

hello_world_test() ->
    {ok, State} = otpcl:eval("set foo {Hello, world!}"),
    {Val, State} = otpcl:get(foo, State),
    ?assertEqual(Val, <<"Hello, world!">>),
    ok.

import_test() ->
    {V1, _State} = otpcl:eval("import erlang; list_to_atom `howdy`"),
    ?assertEqual(V1, howdy),
    {V2, _State} = otpcl:eval("import {erlang}; list_to_atom `howdy`"),
    ?assertEqual(V2, howdy),
    ok.

use_test() ->
    {V1, _State} = otpcl:eval("use erlang; erlang list_to_atom `howdy`"),
    ?assertEqual(V1, howdy),
    {V2, _State} = otpcl:eval("use {erlang}; erlang {list_to_atom} `howdy`"),
    ?assertEqual(V2, howdy),
    ok.

pipe_test() ->
    {ok, State} = otpcl:eval("import erlang"),
    {V1, _State} = otpcl:eval("return `howdy` | list_to_atom", State),
    ?assertEqual(V1, howdy),
    ok.

stringy_test() ->
    S0 = otpcl_env:stringy_state(),
    {ok, S1} = otpcl:import(erlang, S0),
    {V1, _S} = otpcl:eval("return foo | is_atom", S1),
    ?assertEqual(V1, false),
    {V2, _S} = otpcl:eval("return 'foo' | is_atom", S1),
    ?assertEqual(V2, false),
    ok.
