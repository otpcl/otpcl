-module(ecl_parser).

-export([ parse_program/1,
          parse_command/1,
          parse_braced/1,
          parse_double_quoted/1,
          parse_single_quoted/1,
          parse_unquoted/1,
          parse_cmd_sub/1,
          parse_var_sub/1 ]).



parse_program(Txt) -> parse_program(Txt, startpos()).

parse_program(Txt, Pos) when is_binary(Txt) ->
    parse_program(binary_to_list(Txt), Pos);
parse_program(Txt, Pos) when is_list(Txt) ->
    parse_program(Txt, [], Pos);
parse_program(_, _) -> {error, invalid_input}.

parse_program([], Acc, Pos) ->
    {ok, {program, lists:reverse(Acc)}, Pos};
parse_program(Rem, Acc, Pos) ->
    {ok, Cmd, NewRem, NewPos} = parse_command(Rem, Pos),
    parse_program(NewRem, [Cmd|Acc], NewPos).


parse_command(Txt) -> parse_command(Txt, startpos()).
parse_command(Txt, Pos) when is_binary(Txt) ->
    parse_command(binary_to_list(Txt), Pos);
parse_command(Txt, Pos) when is_list(Txt) ->
    parse_command(Txt, [], Pos);
parse_command(_, _) -> {error, invalid_input}.

parse_command(Rem, Acc, Pos) ->
    {ok, {words, Words}, NewRem, NewPos} = parse_words(Acc, Pos),
    {ok, {command, Words}, NewRem, NewPos}.


parse_words(Txt) -> parse_words(Txt, startpos()).

parse_words(Txt, Pos) when is_binary(Txt) ->
    parse_words(binary_to_list(Txt), Pos);
parse_words(Txt, Pos) when is_list(Txt) ->
    parse_words(Txt, [], Pos);
parse_words(_, _) -> {error, invalid_input}.

parse_words([], Acc, Pos) ->
    {ok, {words, lists:reverse(Acc)}, [], Pos};
parse_words([ $\n | Rem ], Acc, Pos) ->
    {ok, {words, lists:reverse(Acc)}, Rem, nextline(Pos)};
parse_words([ $; | Rem ], Acc, Pos) ->
    {ok, {words, lists:reverse(Acc)}, Rem, nextcol(Pos)};
parse_words([ $\s | Rem ], Acc, Pos) ->
    parse_words(Rem, Acc, nextcol(Pos));
parse_words([ $\t | Rem ], Acc, Pos) ->
    parse_words(Rem, Acc, nextcol(Pos));
parse_words([ $\\ | [$\n|Rem] ], Acc, Pos) ->
    parse_words(Rem, Acc, nextline(Pos));
parse_words(Txt, Acc, Pos) ->
    {ok, Word, Rem, NewPos} = parse_word(Txt, Pos),
    parse_words(Rem, [Word|Acc], NewPos).


parse_word(Txt) -> parse_word(Txt, startpos()).
parse_word(Txt, Pos) when is_binary(Txt) ->
    parse_word(binary_to_list(Txt), Pos);
parse_word(Txt, Pos) when is_list(Txt) ->
    parse_word(Txt, [], Pos);
parse_word(_, _) -> {error, invalid_input}.

%parse_word([ $< | Rem ], Acc) ->
%    parse_tuple(Rem, Acc);
%parse_word([ $( | Rem ], Acc) ->
%    parse_list(Rem, Acc);
parse_word([ $# | Rem ], Acc, Pos) ->
    parse_comment(Rem, Acc, nextcol(Pos));
parse_word([ ${ | Rem ], Acc, Pos) ->
    parse_braced(Rem, Acc, nextcol(Pos));
parse_word([ $" | Rem ], Acc, Pos) ->
    parse_double_quoted(Rem, Acc, nextcol(Pos));
parse_word([ $` | Rem ], Acc, Pos) ->
    parse_backquoted(Rem, Acc, nextcol(Pos));
parse_word([ $' | Rem ], Acc, Pos) ->
    parse_single_quoted(Rem, Acc, nextcol(Pos));
parse_word(Txt, Acc, Pos) ->
    parse_unquoted(Txt, Acc, Pos).


parse_comment(Txt) -> parse_comment(Txt, startpos()).

parse_comment(Txt, Pos) when is_binary(Txt) ->
    parse_comment(binary_to_list(Txt), Pos);
parse_comment(Txt, Pos) when is_list(Txt) ->
    parse_comment(Txt, [], Pos);
parse_comment(_, _) -> {error, invalid_input}.

parse_comment([], Acc, Pos) ->
    {ok, {comment, lists:reverse(Acc)}, [], Pos};
parse_comment(Txt = [ $\n | _ ], Acc, Pos) ->
    {ok, {comment, lists:reverse(Acc)}, Txt, Pos};
parse_comment([ C | Rem ], Acc, Pos) ->
    parse_comment(Rem, [C|Acc], nextcol(Pos)).


parse_braced(Txt) -> parse_braced(Txt, startpos()).

parse_braced(Txt, Pos) when is_binary(Txt) ->
    parse_braced(binary_to_list(Txt), Pos);
parse_braced(Txt, Pos) when is_list(Txt) ->
    parse_braced(Txt, [], Pos);
parse_braced(_, _) -> {error, invalid_input}.

parse_braced([], Acc, Pos) ->
    {error, missing_close_brace, Acc, Pos};
parse_braced([ ${ | Rem ], Acc, Pos) ->
    {ok, {string, Inner}, NewRem, NewPos} = parse_braced(Rem, nextcol(Pos)),
    parse_braced(NewRem, "}" ++ lists:reverse(Inner) ++ "{" ++ Acc, NewPos);
parse_braced([ $} | Rem ], Acc, Pos) ->
    {ok, {string, lists:reverse(Acc)}, Rem, nextcol(Pos)};
parse_braced([ C = $\n | Rem ], Acc, Pos) ->
    parse_braced(Rem, [C|Acc], nextline(Pos));
parse_braced([ $\\ | [C|Rem] ], Acc, Pos) ->
    parse_braced(Rem, [C|Acc], nextcol(Pos, 2));
parse_braced([ C | Rem ], Acc, Pos) ->
    parse_braced(Rem, [C|Acc], nextcol(Pos)).


parse_double_quoted(Txt) -> parse_double_quoted(Txt, startpos()).

parse_double_quoted(Txt, Pos) when is_binary(Txt) ->
    parse_double_quoted(binary_to_list(Txt), Pos);
parse_double_quoted(Txt, Pos) when is_list(Txt) ->
    parse_double_quoted(Txt, [], Pos);
parse_double_quoted(_, _) -> {error, invalid_input}.

parse_double_quoted([], Acc, Pos) ->
    {error, missing_double_quote, Acc, Pos};
parse_double_quoted([ $" | Rem ], Acc, Pos) ->
    {ok, {string, lists:reverse(Acc)}, Rem, nextcol(Pos)};
parse_double_quoted([ $[ | Rem ], Acc, Pos) ->
    {ok, CmdSub, NewRem, NewPos} = parse_cmd_sub(Rem, nextcol(Pos)),
    parse_double_quoted(NewRem, [CmdSub|Acc], NewPos);
parse_double_quoted([ $$ | Rem ], Acc, Pos) ->
    {ok, VarSub, NewRem, NewPos} = parse_var_sub(Rem, nextcol(Pos)),
    parse_double_quoted(NewRem, [VarSub|Acc], NewPos);
parse_double_quoted([ C = $\n | Rem ], Acc, Pos) ->
    parse_double_quoted(Rem, [C|Acc], nextline(Pos));
parse_double_quoted([ $\\ | [C|Rem] ], Acc, Pos) ->
    parse_double_quoted(Rem, [C|Acc], nextcol(Pos, 2));
parse_double_quoted([ C | Rem ], Acc, Pos) ->
    parse_double_quoted(Rem, [C|Acc], nextcol(Pos)).


parse_backquoted(Txt) -> parse_backquoted(Txt, startpos()).

parse_backquoted(Txt, Pos) when is_binary(Txt) ->
    parse_backquoted(binary_to_list(Txt), Pos);
parse_backquoted(Txt, Pos) when is_list(Txt) ->
    parse_backquoted(Txt, [], Pos);
parse_backquoted(_, _) -> {error, invalid_input}.

parse_backquoted([], Acc, Pos) ->
    {error, missing_backquote, Acc, Pos};
parse_backquoted([ $` | Rem ], Acc, Pos) ->
    {ok, {charlist, lists:reverse(Acc)}, Rem, nextcol(Pos)};
parse_backquoted([ $[ | Rem ], Acc, Pos) ->
    {ok, CmdSub, NewRem, NewPos} = parse_cmd_sub(Rem, nextcol(Pos)),
    parse_backquoted(NewRem, [CmdSub|Acc], NewPos);
parse_backquoted([ $$ | Rem ], Acc, Pos) ->
    {ok, VarSub, NewRem, NewPos} = parse_var_sub(Rem, nextcol(Pos)),
    parse_backquoted(NewRem, [VarSub|Acc], NewPos);
parse_backquoted([ C = $\n | Rem ], Acc, Pos) ->
    parse_backquoted(Rem, [C|Acc], nextline(Pos));
parse_backquoted([ $\\ | [C|Rem] ], Acc, Pos) ->
    parse_backquoted(Rem, [C|Acc], nextcol(Pos, 2));
parse_backquoted([ C | Rem ], Acc, Pos) ->
    parse_backquoted(Rem, [C|Acc], nextcol(Pos));


parse_single_quoted(Txt) -> parse_single_quoted(Txt, startpos()).

parse_single_quoted(Txt, Pos) when is_binary(Txt) ->
    parse_single_quoted(binary_to_list(Txt), Pos);
parse_single_quoted(Txt, Pos) when is_list(Txt) ->
    parse_single_quoted(Txt, [], Pos);
parse_single_quoted(_, _) -> {error, invalid_input}.

parse_single_quoted([], Acc, Pos) ->
    {error, missing_single_quote, Acc, Pos};
parse_single_quoted([ $' | Rem ], Acc, Pos) ->
    {ok, {atom, lists:reverse(Acc)}, Rem, nextcol(Pos)};
parse_single_quoted([ $[ | Rem ], Acc, Pos) ->
    {ok, CmdSub, NewRem, NewPos} = parse_cmd_sub(Rem, nextcol(Pos)),
    parse_single_quoted(NewRem, [CmdSub|Acc], NewPos);
parse_single_quoted([ $$ | Rem ], Acc, Pos) ->
    {ok, VarSub, NewRem, NewPos} = parse_var_sub(Rem, nextcol(Pos)),
    parse_single_quoted(NewRem, [VarSub|Acc], NewPos);
parse_single_quoted([ C = $\n | Rem ], Acc, Pos) ->
    parse_single_quoted(Rem, [C|Acc], nextline(Pos));
parse_single_quoted([ $\\ | [C|Rem] ], Acc, Pos) ->
    parse_single_quoted(Rem, [C|Acc], nextcol(Pos, 2));
parse_single_quoted([ C | Rem ], Acc, Pos) ->
    parse_single_quoted(Rem, [C|Acc], nextcol(Pos)).


parse_unquoted(Txt) -> parse_unquoted(Txt, startpos()).

parse_unquoted(Txt, Pos) when is_binary(Txt) ->
    parse_unquoted(binary_to_list(Txt), Pos);
parse_unquoted(Txt, Pos) when is_list(Txt) ->
    parse_unquoted(Txt, [], Pos);
parse_unquoted(_, _) -> {error, invalid_input}.

parse_unquoted([], Acc, Pos) ->
    {ok, {atom, lists:reverse(Acc)}, [], Pos};
parse_unquoted([ $\s | Rem ], Acc, Pos) ->
    {ok, {atom, lists:reverse(Acc)}, Rem, nextcol(Pos)};
parse_unquoted(Rem = [ $; | _ ], Acc, Pos) ->
    {ok, {atom, lists:reverse(Acc)}, Rem, Pos};
parse_unquoted(Rem = [ $\n | _ ], Acc, Pos) ->
    {ok, {atom, lists:reverse(Acc)}, Rem, Pos};
parse_unquoted([ $[ | Rem ], Acc, Pos) ->
    {ok, CmdSub, NewRem, NewPos} = parse_cmd_sub(Rem, nextcol(Pos)),
    parse_unquoted(NewRem, [CmdSub|Acc], NewPos);
parse_unquoted([ $$ | Rem ], Acc, Pos) ->
    {ok, VarSub, NewRem, NewPos} = parse_var_sub(Rem, nextcol(Pos)),
    parse_unquoted(NewRem, [VarSub|Acc], NewPos);
parse_unquoted([ $\\ | [C|Rem] ], Acc, Pos) ->
    parse_unquoted(Rem, [C|Acc], nextcol(Pos, 2));
parse_unquoted([ C | Rem ], Acc, Pos) ->
    parse_unquoted(Rem, [C|Acc], nextcol(Pos)).


parse_cmd_sub(Txt) -> parse_cmd_sub(Txt, startpos()).

parse_cmd_sub(Txt, Pos) when is_binary(Txt) ->
    parse_cmd_sub(binary_to_list(Txt), Pos);
parse_cmd_sub(Txt, Pos) when is_list(Txt) ->
    parse_cmd_sub(Txt, [], Pos);
parse_cmd_sub(_, _) -> {error, invalid_input}.

parse_cmd_sub([], Acc, Pos) ->
    {error, missing_close_bracket, Acc, Pos};
parse_cmd_sub([ $[ | Rem ], Acc, Pos) ->
    {ok, CmdSub, NewRem, NewPos} = parse_cmd_sub(Rem, nextcol(Pos)),
    parse_cmd_sub(NewRem, [CmdSub|Acc], NewPos);
parse_cmd_sub([ $] | Rem ], Acc, Pos) ->
    {ok, {cmd_sub, lists:reverse(Acc)}, Rem, nextcol(Pos)};
parse_cmd_sub([ $$ | Rem ], Acc, Pos) ->
    {ok, VarSub, NewRem, NewPos} = parse_var_sub(Rem, nextcol(Pos)),
    parse_cmd_sub(NewRem, [VarSub|Acc], NewPos);
parse_cmd_sub([ C = $\n | Rem ], Acc, Pos) ->
    parse_cmd_sub(Rem, [C|Acc], nextline(Pos));
parse_cmd_sub([ $\\ | [C|Rem] ], Acc, Pos) ->
    parse_cmd_sub(Rem, [C|Acc], nextcol(Pos, 2));
parse_cmd_sub([ C | Rem ], Acc, Pos) ->
    parse_cmd_sub(Rem, [C|Acc], nextcol(Pos)).


parse_var_sub(Txt) -> parse_var_sub(Txt, startpos()).

parse_var_sub(Txt, Pos) when is_binary(Txt) ->
    parse_var_sub(binary_to_list(Txt), Pos);
parse_var_sub([ ${ | Txt ], Pos) ->
    parse_braced_var_sub(Txt, nextcol(Pos));
parse_var_sub(Txt, Pos) when is_list(Txt) ->
    parse_var_sub(Txt, [], Pos);
parse_var_sub(_, _) -> {error, invalid_input}.

parse_var_sub([], Acc, Pos) ->
    {ok, {var_sub, lists:reverse(Acc)}, [], Pos};
parse_var_sub(Txt = [ $$ | _ ], Acc, Pos) ->
    {ok, {var_sub, lists:reverse(Acc)}, Txt, Pos};
parse_var_sub(Txt = [ $[ | _ ], Acc, Pos) ->
    {ok, {var_sub, lists:reverse(Acc)}, Txt, Pos};
parse_var_sub(Txt = [ $] | _ ], Acc, Pos) ->
    {ok, {var_sub, lists:reverse(Acc)}, Txt, Pos};
parse_var_sub(Txt = [ $\s | _ ], Acc, Pos) ->
    {ok, {var_sub, lists:reverse(Acc)}, Txt, Pos};
parse_var_sub(Txt = [ $; | _ ], Acc, Pos) ->
    {ok, {var_sub, lists:reverse(Acc)}, Txt, Pos};
parse_var_sub(Txt = [ $\n | _ ], Acc, Pos) ->
    {ok, {var_sub, lists:reverse(Acc)}, Txt, Pos};
parse_var_sub([ $\\ | [C|Rem] ], Acc, Pos) ->
    parse_var_sub(Rem, [C|Acc], nextcol(Pos, 2));
parse_var_sub([ C | Rem ], Acc, Pos) ->
    parse_var_sub(Rem, [C|Acc], nextcol(Pos)).

parse_braced_var_sub(Txt, Pos) ->
    {ok, {string, VarName}, Rem, NewPos} = parse_braced(Txt, Pos),
    {ok, {var_sub, VarName}, Rem, NewPos}.



% Helpers

startpos() -> {nofile,0,0}.

nextline(Pos) -> nextline(Pos, 1).
nextcol(Pos) -> nextcol(Pos, 1).

nextline({F,L,C}, N}) -> {F,L+N,C}.
nextcol({F,L,C}, N}) -> {F,L,C+N}.