-module(otpcl_env_tests).
-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

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

stringy_switch_test() ->
    Script = string:join(["import erlang",
                          "set pre_stringy [is_atom foo]",
                          "set STRINGY_INTERPRETER ok",
                          "set stringy [is_atom foo]",
                          "unset STRINGY_INTERPRETER",
                          "set post_stringy [is_atom foo]",
                          "return <$pre_stringy $stringy $post_stringy>"],
                         "\n"),
    {{PreStringy, Stringy, PostStringy}, S0} = otpcl:eval(Script),
    ?assertEqual(PreStringy, true),
    ?assertEqual(Stringy, false),
    ?assertEqual(PostStringy, true),
    ok.
