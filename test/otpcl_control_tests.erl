-module(otpcl_control_tests).
-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

if_true_test() ->
    {V1, _S1} = otpcl:eval("if {foo} {return pass} {return fail}"),
    ?assertEqual(V1, pass),
    ok.
if_false_test() ->
    {V1, _S1} = otpcl:eval("if {} {return fail} {return pass}"),
    ?assertEqual(V1, pass),
    ok.

unless_test() ->
    {V1, _S1} = otpcl:eval("return pass; unless {foo} {return fail}"),
    ?assertEqual(V1, pass),
    {V2, _S2} = otpcl:eval("return fail; unless {} {return pass}"),
    ?assertEqual(V2, pass),
    ok.

for_test() ->
    {_, S1} = otpcl:eval("set list ({fail} {fail} {pass})"),
    {_, S2} = otpcl:eval("import erlang", S1),
    {V3, _S3} = otpcl:eval("for i in $list {binary_to_atom $i}", S2),
    ?assertEqual(V3, pass),
    ok.

while_test() ->
    {_, S1} = otpcl:eval("set condition true"),
    {_, _S2} = otpcl:eval("while {get condition} {set condition false}", S1),
    ok.

break_test() ->
    {_, _S1} = otpcl:eval("while {return true} {break}"),
    ok.
