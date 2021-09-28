-module(otpcl_meta_tests).
-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

import_test() ->
    {V1, _S1} = otpcl:eval("import erlang; list_to_atom `howdy`"),
    ?assertEqual(V1, howdy),
    {V2, _S2} = otpcl:eval("import {erlang}; list_to_atom `howdy`"),
    ?assertEqual(V2, howdy),
    ok.

use_test() ->
    {V1, _S1} = otpcl:eval("use erlang; erlang list_to_atom `howdy`"),
    ?assertEqual(V1, howdy),
    {V2, _S2} = otpcl:eval("use {erlang}; erlang {list_to_atom} `howdy`"),
    ?assertEqual(V2, howdy),
    ok.

subcmd_test() ->
    S0 = otpcl_env:default_state(),
    ToList = fun ([List], State) when is_list(List) ->
                     {List, State};
                 ([Binary], State) when is_binary(Binary) ->
                     {binary_to_list(Binary), State};
                 ([Atom], State) when is_atom(Atom) ->
                     {atom_to_list(Atom), State}
             end,
    ToBin  = fun ([List], State) when is_list(List) ->
                     {list_to_binary(List), State};
                 ([Binary], State) when is_binary(Binary) ->
                     {Binary, State};
                 ([Atom], State) when is_atom(Atom) ->
                     {atom_to_binary(Atom, utf8), State}
             end,
    ToAtom = fun ([List], State) when is_list(List) ->
                     {list_to_atom(List), State};
                 ([Binary], State) when is_binary(Binary) ->
                     {binary_to_atom(Binary), State};
                 ([Atom], State) when is_atom(Atom) ->
                     {Atom, State}
             end,
    {V1, S1} = otpcl_meta:subcmd([list, ToList, bin, ToBin, atom, ToAtom], S0),
    {_V2, S2} = otpcl_meta:cmd(convert_to, V1, S1),
    {V3, _S3} = otpcl:eval("convert_to list foo", S2),
    ?assertEqual("foo", V3),
    {V4, _S4} = otpcl:eval("convert_to bin foo", S2),
    ?assertEqual(<<"foo">>, V4),
    {V5, _S5} = otpcl:eval("convert_to atom foo", S2),
    ?assertEqual(foo, V5),
    {V6, _S6} = otpcl:eval("convert_to list {foo}", S2),
    ?assertEqual("foo", V6),
    {V7, _S7} = otpcl:eval("convert_to atom `foo`", S2),
    ?assertEqual(foo, V7),
    % I think you get the point by now...
    ok.

cmd_test() ->
    S0 = otpcl_env:default_state(),
    SumFun = fun (Nums, State) ->
                     {lists:sum(Nums), State}
             end,
    {_V1, S1} = otpcl_meta:cmd(sum, SumFun, S0),
    {V2, _S2} = otpcl:eval("sum 1 2 3 4 5", S1),
    ?assertEqual(15, V2),
    ok.
