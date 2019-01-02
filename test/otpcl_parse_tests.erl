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
    [{parsed, command, AtomNodes}] = Cmds,
    [foo, bar, baz] = [interpret(N) || N <- AtomNodes],
    ok.

parse_binstrings_test() ->
    {ok, {parsed, program, Cmds}, []} = otpcl_parse:parse("\"foo\" {bar}"),
    [{parsed, command, StringNodes}] = Cmds,
    [<<"foo">>, <<"bar">>] = [interpret(N) || N <- StringNodes],
    ok.

parse_charstrings_test() ->
    {ok, {parsed, program, Cmds}, []} = otpcl_parse:parse("`foo` `bar`"),
    [{parsed, command, CharstringNodes}] = Cmds,
    ["foo", "bar"] = [interpret(N) || N <- CharstringNodes],
    ok.

parse_lists_test() ->
    {ok, {parsed, program, Cmds}, []} = otpcl_parse:parse("(foo (bar (baz)))"),
    [{parsed, command, [ListNode]}] = Cmds,
    [foo, [bar, [baz]]] = interpret(ListNode),
    ok.

parse_tuple_test() ->
    {ok, {parsed, program, Cmds}, []} = otpcl_parse:parse("<foo bar baz>"),
    [{parsed, command, [TupleNode]}] = Cmds,
    {foo, bar, baz} = interpret(TupleNode),
    ok.

parse_variables_test() ->
    {ok, {parsed, program, Cmds}, []} = otpcl_parse:parse("$foo ${bar}"),
    [{parsed, command, [First, Second]}] = Cmds,
    "Value of foo" = interpret(First),
    "Value of bar" = interpret(Second),
    ok.

example_function(First, Second, Third) ->
    {First, Second, Third}.
example_function_2(First, Second, Third) ->
    [First, Second, Third].

parse_funcall_test() ->
    SrcCode = "[example_function foo bar baz]",
    {ok, {parsed, program, Cmds}, []} = otpcl_parse:parse(SrcCode),
    [{parsed, command, [FuncallNode]}] = Cmds,
    {foo, bar, baz} = interpret(FuncallNode),
    ok.

parse_commands_test() ->
    SrcCode =
        "example_function foo bar baz\nexample_function_2 baz bar foo",
    {ok, {parsed, program, Cmds}, []} = otpcl_parse:parse(SrcCode),
    [First, Second] = [interpret(C) || C <- Cmds],
    {foo, bar, baz} = First,
    [baz, bar, foo] = Second,
    ok.

parse_continued_command_test() ->
    SrcCode = "example_function foo bar\\\n    baz",
    {ok, {parsed, program, [Cmd]}, []} = otpcl_parse:parse(SrcCode),
    {foo, bar, baz} = interpret(Cmd),
    ok.

parse_semicoloned_commands_test() ->
    SrcCode =
        "example_function foo bar baz; example_function_2 baz bar foo",
    {ok, {parsed, program, Cmds}, []} = otpcl_parse:parse(SrcCode),
    [First, Second] = [interpret(C) || C <- Cmds],
    {foo, bar, baz} = First,
    [baz, bar, foo] = Second,
    ok.

parse_comments_test() ->
    SrcCode =
        "example_function foo bar baz # This is a comment",
    {ok, {parsed, program, Cmds}, []} = otpcl_parse:parse(SrcCode),
    [{foo,bar,baz}, " This is a comment"] = [interpret(C) || C <- Cmds],
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

%%% TODO: this might actually *be* the interpreter, though vars and
%%% funcalls will need to be handled within token lists at some point,
%%% and we'll probably want to track interpretation state, but
%%% whatever.

interpret({parsed, unquoted, Tokens}) ->
    make_atom(Tokens);
interpret({parsed, single_quoted, Tokens}) ->
    make_atom(Tokens);
interpret({parsed, double_quoted, Tokens}) ->
    make_binstring(Tokens);
interpret({parsed, braced, Tokens}) ->
    make_binstring(Tokens);
interpret({parsed, backquoted, Tokens}) ->
    make_charstring(Tokens);
interpret({parsed, list, Items}) ->
    [interpret(I) || I <- Items];
interpret({parsed, tuple, Items}) ->
    list_to_tuple([interpret(I) || I <- Items]);
interpret({parsed, funcall, Words}) ->
    [Name|Args] = [interpret(I) || I <- Words],
    apply(?MODULE, Name, Args);
interpret({parsed, command, Words}) ->
    [Name|Args] = [interpret(I) || I <- Words],
    apply(?MODULE, Name, Args);
interpret({parsed, var_unquoted, Tokens}) ->
    % Since this is not the real interpreter, we're just going to mock
    % this.
    "Value of " ++ make_charstring(Tokens);
interpret({parsed, var_braced, Tokens}) ->
    % See above
    "Value of " ++ make_charstring(Tokens);
interpret({parsed, comment, Tokens}) ->
    % This ain't what the interpreter will actually do, but it's good
    % enough for our tests.
    make_charstring(Tokens);
interpret({parsed, Type, Data}) ->
    {error, unknown_node_type, Type, Data};
interpret(_) ->
    {error, not_an_otpcl_parse_node}.
