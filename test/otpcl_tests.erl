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
    {ok, Normal} = otpcl:import(erlang, otpcl_env:core_state()),
    {ok, Stringy} = otpcl:import(erlang, otpcl_env:stringy_state()),
    {V1N, _} = otpcl:eval("return foo | is_atom", Normal),
    {V1S, _} = otpcl:eval("return foo | is_atom", Stringy),
    ?assertEqual(V1N, true),
    ?assertEqual(V1S, false),
    {V2N, _} = otpcl:eval("return 'foo' | is_atom", Normal),
    {V2S, _} = otpcl:eval("return 'foo' | is_atom", Stringy),
    ?assertEqual(V2N, true),
    ?assertEqual(V2S, false),
    {V3N, _} = otpcl:eval("return 123 | is_integer", Normal),
    {V3S, _} = otpcl:eval("return 123 | is_integer", Stringy),
    ?assertEqual(V3N, true),
    ?assertEqual(V3S, false),
    {V4N, _} = otpcl:eval("return 1.23 | is_float", Normal),
    {V4S, _} = otpcl:eval("return 1.23 | is_float", Stringy),
    ?assertEqual(V4N, true),
    ?assertEqual(V4S, false),
    ok.
