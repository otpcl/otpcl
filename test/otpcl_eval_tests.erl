-module(otpcl_eval_tests).
-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

eval_test() ->
    {p, _S1} = otpcl_eval:eval("return p"),
    {p, _S2} = otpcl_eval:eval("return p", otpcl_env:core_state()),
    {p, _S3} = otpcl_eval:'CMD_eval'(["return p"], otpcl_env:core_state()),
    ok.

eval_file_test() ->
    F = "test/eval_file_test.otpcl",
    {{p,p}, _S1} = otpcl_eval:eval_file(F),
    {{p,p}, _S2} = otpcl_eval:eval_file(F, otpcl_env:default_state()),
    {{p,p}, _S3} = otpcl_eval:'CMD_eval_file'([F], otpcl_env:default_state()),
    ok.
