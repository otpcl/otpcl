-module(otpcl_parser_SUITE).
-compile(export_all).

-include("ct.hrl").

all() ->
    [scan_foo,
     scan_foo_line_bar,
     parse_foo,
     parse_atoms,
     parse_binstrings].

init_per_suite(Config) -> Config.
end_per_suite(Config) -> Config.
init_per_testcase(_TestCase, Config) -> Config.
end_per_testcase(_TestCase, Config) -> Config.

scan_foo(_) ->
    [{$f, {nofile,0,0}},
     {$o, {nofile,0,1}},
     {$o, {nofile,0,2}}] = otpcl_parser:scan("foo"),
    ok.

scan_foo_line_bar(_) ->
    [{$f, {nofile,0,0}},
     {$o, {nofile,0,1}},
     {$o, {nofile,0,2}},
     {$\n, {nofile,0,3}},
     {$b, {nofile,1,0}},
     {$a, {nofile,1,1}},
     {$r, {nofile,1,2}}] = otpcl_parser:scan("foo\nbar"),
    ok.

parse_foo(_) ->
    {ok, {parsed, program, Cmds}, []} = otpcl_parser:parse("foo"),
    [{parsed, command, Words}] = Cmds,
    [{parsed, unquoted, Tokens}] = Words,
    [{$f, {nofile,0,0}},
     {$o, {nofile,0,1}},
     {$o, {nofile,0,2}}] = Tokens,
    ok.

parse_atoms(_) ->
    {ok, {parsed, program, Cmds}, []} = otpcl_parser:parse("foo bar baz"),
    [{parsed, command, Words}] = Cmds,
    [{parsed, unquoted, First},
     {parsed, unquoted, Second},
     {parsed, unquoted, Third}] = Words,
    foo = make_atom(First),
    bar = make_atom(Second),
    baz = make_atom(Third),
    ok.

parse_binstrings(_) ->
    {ok, {parsed, program, Cmds}, []} = otpcl_parser:parse("\"foo\" {bar}"),
    [{parsed, command, Words}] = Cmds,
    [{parsed, double_quoted, First},
     {parsed, braced, Second}] = Words,
    <<"foo">> = make_binstring(First),
    <<"bar">> = make_binstring(Second),
    ok.



%% HELPERS

% TODO: these will eventually be part of the interpreter (or be the
% foundation for the implementations of similar functions in the
% actual interpreter).

make_atom(Tokens) ->
    list_to_atom([C || {C,_} <- Tokens]).

make_binstring(Tokens) ->
    list_to_binary([C || {C,_} <- Tokens]).
