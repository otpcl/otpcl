-module(otpcl_parse_tests).
-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

scan_foo_test() ->
    [{$f, {nofile,0,0}},
     {$o, {nofile,0,1}},
     {$o, {nofile,0,2}}] = otpcl_parse:scan("foo"),
    ok.

scan_foo_line_bar_test() ->
    [{$f, {nofile,0,0}},
     {$o, {nofile,0,1}},
     {$o, {nofile,0,2}},
     {$\n, {nofile,0,3}},
     {$b, {nofile,1,0}},
     {$a, {nofile,1,1}},
     {$r, {nofile,1,2}}] = otpcl_parse:scan("foo\nbar"),
    ok.

parse_foo_test() ->
    {ok, {parsed, program, Cmds}, []} = otpcl_parse:parse("foo"),
    [{parsed, command, Words}] = Cmds,
    [{parsed, unquoted, Tokens}] = Words,
    [{$f, {nofile,0,0}},
     {$o, {nofile,0,1}},
     {$o, {nofile,0,2}}] = Tokens,
    ok.

parse_atoms_test() ->
    {ok, {parsed, program, Cmds}, []} = otpcl_parse:parse("foo 'bar' baz"),
    [{parsed, command, Words}] = Cmds,
    [{parsed, unquoted, First},
     {parsed, single_quoted, Second},
     {parsed, unquoted, Third}] = Words,
    foo = make_atom(First),
    bar = make_atom(Second),
    baz = make_atom(Third),
    ok.

parse_binstrings_test() ->
    {ok, {parsed, program, Cmds}, []} = otpcl_parse:parse("\"foo\" {bar}"),
    [{parsed, command, Words}] = Cmds,
    [{parsed, double_quoted, First},
     {parsed, braced, Second}] = Words,
    <<"foo">> = make_binstring(First),
    <<"bar">> = make_binstring(Second),
    ok.

parse_charstrings_test() ->
    {ok, {parsed, program, Cmds}, []} = otpcl_parse:parse("`foo` `bar`"),
    [{parsed, command, Words}] = Cmds,
    [{parsed, backquoted, First},
     {parsed, backquoted, Second}] = Words,
    "foo" = make_charstring(First),
    "bar" = make_charstring(Second),
    ok.

parse_lists_test() ->
    {ok, {parsed, program, Cmds}, []} = otpcl_parse:parse("(foo (bar (baz)))"),
    [{parsed, command, Words}] = Cmds,
    [{parsed, list, Items}] = Words,
    [{parsed, unquoted, First}|Rest] = Items,
    [{parsed, list, RestItems}] = Rest,
    [{parsed, unquoted, Second}|RestRest] = RestItems,
    [{parsed, list, RestRestItems}] = RestRest,
    [{parsed, unquoted, Third}] = RestRestItems,
    foo = make_atom(First),
    bar = make_atom(Second),
    baz = make_atom(Third),
    ok.


%% HELPERS

% TODO: these will eventually be part of the interpreter (or be the
% foundation for the implementations of similar functions in the
% actual interpreter).

make_atom(Tokens) ->
    list_to_atom(make_charstring(Tokens)).

make_binstring(Tokens) ->
    list_to_binary(make_charstring(Tokens)).

make_charstring(Tokens) ->
    [C || {C,_} <- Tokens].
