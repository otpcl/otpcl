-module(ecl_parser).

-export([ parse_program/1,
          parse_command/1,
          parse_braced/1,
          parse_double_quoted/1,
          parse_single_quoted/1,
          parse_unquoted/1,
          parse_cmd_sub/1,
          parse_var_sub/1 ]).


parse_program(Txt) when is_binary(Txt) ->
    parse_program(binary_to_list(Txt));
parse_program(Txt) when is_list(Txt) ->
    parse_program(Txt, []);
parse_program(_) -> {error, invalid_input}.

parse_program([], Acc) ->
    {ok, {program, lists:reverse(Acc)}};
parse_program(Rem, Acc) ->
    {ok, Cmd, NewRem} = parse_command(Rem),
    parse_program(NewRem, [Cmd|Acc]).



parse_command(Txt) when is_binary(Txt) ->
    parse_command(binary_to_list(Txt));
parse_command(Txt) when is_list(Txt) ->
    parse_command(Txt, []);
parse_command(_) -> {error, invalid_input}.

parse_command(Rem, Acc) ->
    {ok, {words, Words}, NewRem} = parse_words(Acc),
    {ok, {command, Words}, NewRem}.



parse_words(Txt) when is_binary(Txt) ->
    parse_words(binary_to_list(Txt));
parse_words(Txt) when is_list(Txt) ->
    parse_words(Txt, []);
parse_words(_) -> {error, invalid_input}.

parse_words([], Acc) ->
    {ok, {words, lists:reverse(Acc)}, []};
parse_words([ $\n | Rem ], Acc) ->
    {ok, {words, lists:reverse(Acc)}, Rem};
parse_words([ $; | Rem ], Acc) ->
    {ok, {words, lists:reverse(Acc)}, Rem};
parse_words([ $\s | Rem ], Acc) ->
    parse_words(Rem, Acc);
parse_words([ $\t | Rem ], Acc) ->
    parse_words(Rem, Acc);
parse_words([ $\\ | [$\n|Rem] ], Acc) ->
    parse_words(Rem, Acc);
parse_words(Rem, Acc) ->
    {ok, Word, NewRem} = parse_word(Rem),
    parse_words(NewRem, [Word|Acc]).



parse_word(Txt) when is_binary(Txt) ->
    parse_word(binary_to_list(Txt));
parse_word(Txt) when is_list(Txt) ->
    parse_word(Txt, []);
parse_word(_) -> {error, invalid_input}.

%parse_word([ $< | Rem ], Acc) ->
%    parse_tuple(Rem, Acc);
%parse_word([ $( | Rem ], Acc) ->
%    parse_list(Rem, Acc);
parse_word([ $# | Rem ], Acc) ->
    parse_comment(Rem, Acc);
parse_word([ ${ | Rem ], Acc) ->
    parse_braced(Rem, Acc);
parse_word([ $" | Rem ], Acc) ->
    parse_double_quoted(Rem, Acc);
parse_word([ $` | Rem ], Acc) ->
    parse_backquoted(Rem, Acc);
parse_word([ $' | Rem ], Acc) ->
    parse_single_quoted(Rem, Acc);
parse_word(Rem, Acc) ->
    parse_unquoted(Rem, Acc).



parse_comment(Txt) when is_binary(Txt) ->
    parse_comment(binary_to_list(Txt));
parse_comment(Txt) when is_list(Txt) ->
    parse_comment(Txt, []);
parse_comment(_) -> {error, invalid_input}.

parse_comment([], Acc) ->
    {ok, {comment, lists:reverse(Acc)}, []};
parse_comment(Rem = [ $\n | _ ], Acc) ->
    {ok, {comment, lists:reverse(Acc)}, Rem};
parse_comment([ C | Rem ], Acc) ->
    parse_comment(Rem, [C|Acc]).



parse_braced(Txt) when is_binary(Txt) ->
    parse_braced(binary_to_list(Txt));
parse_braced(Txt) when is_list(Txt) ->
    parse_braced(Txt, []);
parse_braced(_) -> {error, invalid_input}.

parse_braced([], Acc) ->
    {error, missing_close_brace, Acc};
parse_braced([ ${ | Rem ], Acc) ->
    {ok, {string, Inner}, NewRem} = parse_braced(Rem),
    parse_braced(NewRem, "}" ++ lists:reverse(Inner) ++ "{" ++ Acc);
parse_braced([ $} | Rem ], Acc) ->
    {ok, {string, lists:reverse(Acc)}, Rem};
parse_braced([ $\\ | [C|Rem] ], Acc) ->
    parse_braced(Rem, [C|Acc]);
parse_braced([ C | Rem ], Acc) ->
    parse_braced(Rem, [C|Acc]).



parse_double_quoted(Txt) when is_binary(Txt) ->
    parse_double_quoted(binary_to_list(Txt));
parse_double_quoted(Txt) when is_list(Txt) ->
    parse_double_quoted(Txt, []);
parse_double_quoted(_) -> {error, invalid_input}.

parse_double_quoted([], Acc) ->
    {error, missing_double_quote, Acc};
parse_double_quoted([ $" | Rem ], Acc) ->
    {ok, {string, lists:reverse(Acc)}, Rem};
parse_double_quoted([ $[ | Rem ], Acc) ->
    {ok, CmdSub, NewRem} = parse_cmd_sub(Rem),
    parse_double_quoted(NewRem, [CmdSub|Acc]);
parse_double_quoted([ $$ | Rem ], Acc) ->
    {ok, VarSub, NewRem} = parse_var_sub(Rem),
    parse_double_quoted(NewRem, [VarSub|Acc]);
parse_double_quoted([ $\\ | [C|Rem] ], Acc) ->
    parse_double_quoted(Rem, [C|Acc]);
parse_double_quoted([ C | Rem ], Acc) ->
    parse_double_quoted(Rem, [C|Acc]).



parse_backquoted(Txt) when is_binary(Txt) ->
    parse_backquoted(binary_to_list(Txt));
parse_backquoted(Txt) when is_list(Txt) ->
    parse_backquoted(Txt, []);
parse_backquoted(_) -> {error, invalid_input}.

parse_backquoted([], Acc) ->
    {error, missing_backquote, Acc};
parse_backquoted([ $` | Rem ], Acc) ->
    {ok, {charlist, lists:reverse(Acc)}, Rem};
parse_backquoted([ $[ | Rem ], Acc) ->
    {ok, CmdSub, NewRem} = parse_cmd_sub(Rem),
    parse_backquoted(NewRem, [CmdSub|Acc]);
parse_backquoted([ $$ | Rem ], Acc) ->
    {ok, VarSub, NewRem} = parse_var_sub(Rem),
    parse_backquoted(NewRem, [VarSub|Acc]);
parse_backquoted([ $\\ | [C|Rem] ], Acc) ->
    parse_backquoted(Rem, [C|Acc]);
parse_backquoted([ C | Rem ], Acc) ->
    parse_backquoted(Rem, [C|Acc]);



parse_single_quoted(Txt) when is_binary(Txt) ->
    parse_single_quoted(binary_to_list(Txt));
parse_single_quoted(Txt) when is_list(Txt) ->
    parse_single_quoted(Txt, []);
parse_single_quoted(_) -> {error, invalid_input}.

parse_single_quoted([], Acc) ->
    {error, missing_single_quote, Acc};
parse_single_quoted([ $' | Rem ], Acc) ->
    {ok, {atom, lists:reverse(Acc)}, Rem};
parse_single_quoted([ $[ | Rem ], Acc) ->
    {ok, CmdSub, NewRem} = parse_cmd_sub(Rem),
    parse_single_quoted(NewRem, [CmdSub|Acc]);
parse_single_quoted([ $$ | Rem ], Acc) ->
    {ok, VarSub, NewRem} = parse_var_sub(Rem),
    parse_single_quoted(NewRem, [VarSub|Acc]);
parse_single_quoted([ $\\ | [C|Rem] ], Acc) ->
    parse_single_quoted(Rem, [C|Acc]);
parse_single_quoted([ C | Rem ], Acc) ->
    parse_single_quoted(Rem, [C|Acc]).



parse_unquoted(Txt) when is_binary(Txt) ->
    parse_unquoted(binary_to_list(Txt));
parse_unquoted(Txt) when is_list(Txt) ->
    parse_unquoted(Txt, []);
parse_unquoted(_) -> {error, invalid_input}.

parse_unquoted([], Acc) ->
    {ok, {atom, lists:reverse(Acc)}, []};
parse_unquoted([ $\s | Rem ], Acc) ->
    {ok, {atom, lists:reverse(Acc)}, Rem};
parse_unquoted(Rem = [ $; | _ ], Acc) ->
    {ok, {atom, lists:reverse(Acc)}, Rem};
parse_unquoted(Rem = [ $\n | _ ], Acc) ->
    {ok, {atom, lists:reverse(Acc)}, Rem};
parse_unquoted([ $[ | Rem ], Acc) ->
    {ok, CmdSub, NewRem} = parse_cmd_sub(Rem),
    parse_unquoted(NewRem, [CmdSub|Acc]);
parse_unquoted([ $$ | Rem ], Acc) ->
    {ok, VarSub, NewRem} = parse_var_sub(Rem),
    parse_unquoted(NewRem, [VarSub|Acc]);
parse_unquoted([ $\\ | [C|Rem] ], Acc) ->
    parse_unquoted(Rem, [C|Acc]);
parse_unquoted([ C | Rem ], Acc) ->
    parse_unquoted(Rem, [C|Acc]).



parse_cmd_sub(Txt) when is_binary(Txt) ->
    parse_cmd_sub(binary_to_list(Txt));
parse_cmd_sub(Txt) when is_list(Txt) ->
    parse_cmd_sub(Txt, []);
parse_cmd_sub(_) -> {error, invalid_input}.

parse_cmd_sub([], Acc) ->
    {error, missing_close_bracket, Acc};
parse_cmd_sub([ $[ | Rem ], Acc) ->
    {ok, CmdSub, NewRem} = parse_cmd_sub(Rem),
    parse_cmd_sub(NewRem, [CmdSub|Acc]);
parse_cmd_sub([ $] | Rem ], Acc) ->
    {ok, {cmd_sub, lists:reverse(Acc)}, Rem};
parse_cmd_sub([ $$ | Rem ], Acc) ->
    {ok, VarSub, NewRem} = parse_var_sub(Rem),
    parse_cmd_sub(NewRem, [VarSub|Acc]);
parse_cmd_sub([ $\\ | [C|Rem] ], Acc) ->
    parse_cmd_sub(Rem, [C|Acc]);
parse_cmd_sub([ C | Rem ], Acc) ->
    parse_cmd_sub(Rem, [C|Acc]).



parse_var_sub(Txt) when is_binary(Txt) ->
    parse_var_sub(binary_to_list(Txt));
parse_var_sub([ ${ | Txt ]) ->
    parse_braced_var_sub(Txt);
parse_var_sub(Txt) when is_list(Txt) ->
    parse_var_sub(Txt, []);
parse_var_sub(_) -> {error, invalid_input}.

parse_var_sub([], Acc) ->
    {ok, {var_sub, lists:reverse(Acc)}, []};
parse_var_sub(Txt = [ $$ | _ ], Acc) ->
    {ok, {var_sub, lists:reverse(Acc)}, Txt};
parse_var_sub(Txt = [ $[ | _ ], Acc) ->
    {ok, {var_sub, lists:reverse(Acc)}, Txt};
parse_var_sub(Txt = [ $] | _ ], Acc) ->
    {ok, {var_sub, lists:reverse(Acc)}, Txt};
parse_var_sub(Txt = [ $\s | _ ], Acc) ->
    {ok, {var_sub, lists:reverse(Acc)}, Txt};
parse_var_sub(Txt = [ $; | _ ], Acc) ->
    {ok, {var_sub, lists:reverse(Acc)}, Txt};
parse_var_sub(Txt = [ $\n | _ ], Acc) ->
    {ok, {var_sub, lists:reverse(Acc)}, Txt};
parse_var_sub([ $\\ | [C|Rem] ], Acc) ->
    parse_var_sub(Rem, [C|Acc]);
parse_var_sub([ C | Rem ], Acc) ->
    parse_var_sub(Rem, [C|Acc]).

parse_braced_var_sub(Txt) ->
    {ok, {string, VarName}, Rem} = parse_braced(Txt),
    {ok, {var_sub, VarName}, Rem}.
