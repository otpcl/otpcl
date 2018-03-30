-module(otpcl_parser).

-export([ parse_program/1,
          parse_command/1,
          parse_braced/1,
          parse_double_quoted/1,
          parse_single_quoted/1,
          parse_unquoted/1,
          parse_cmd_sub/1,
          parse_var_sub/1 ]).



parse(Txt) -> parse([program], Txt, initpos()).

parse(Lvls, Txt) -> parse(Lvls, Txt, [], initpos()).

parse(Lvls, Txt, Acc, Pos) when is_binary(Txt) ->
    parse(Lvls, binary_to_list(Txt), Acc, Pos);



% Program / toplevel

parse([program|Up], Txt, Acc, Pos) ->
    {ok, {body, Body}, Rem, NewPos} = parse([body|Up], Txt, [], Pos),
    {ok, {program, Body}, Rem, NewPos};



% Generic "body"

parse([body|_], [], Acc, Pos) ->
    {ok, {body, lists:reverse(Acc)}, [], Pos};
parse(Lvls = [body|_], Txt, Acc, Pos) ->
    {ok, Cmd, Rem, NewPos} = parse([command|Lvls], Txt, [], Pos),
    parse(Lvls, Rem, [Cmd|Acc], NewPos);



% Commands and their wordlists

parse([command|Up], Txt, Acc, Pos) ->
    {ok, {words, W}, Rem, NewPos} = parse([words|Up], Txt, Acc, Pos),
    {ok, {command, W}, Rem, NewPos};

parse([words|[command|_]], [$\n|Rem], Acc, Pos) ->
    {ok, {words, lists:reverse(Acc)}, Rem, nextline(Pos)};
parse([words|[command|_]], [$;|Rem], Acc, Pos) ->
    {ok, {words, lists:reverse(Acc)}, Rem, nextcol(Pos)};
parse(Lvls = [words|[command|_]], [$\\|[$\n|Rem]], Acc, Pos) ->
    parse(Lvls, Rem, Acc, nextline(Pos));



% Generic wordlists

parse([words|_], [], Acc, Pos) ->
    {ok, {words, lists:reverse(Acc)}, [], Pos};
parse(Lvls = [words|_], [$\s|Rem], Acc, Pos) ->
    parse(Lvls, Rem, Acc, nextcol(Pos));
parse(Lvls = [words|_], [$\t|Rem], Acc, Pos) ->
    parse(Lvls, Rem, Acc, nextcol(Pos));
parse(Lvls = [words|Up], Txt, Acc, Pos) ->
    {ok, Word, Rem, NewPos} = parse([word|Up], Txt, [], Pos),
    parse(Lvls, Rem, [Word|Acc], NewPos);



% Word dispatcher

parse([word|Up], [$#|Rem], Acc, Pos) ->
    parse([comment|Up], Rem, Acc, nextcol(Pos));
parse([word|Up], [${|Rem], Acc, Pos) ->
    parse([braced|Up], Rem, Acc, nextcol(Pos));
parse([word|Up], [$"|Rem], Acc, Pos) ->
    parse([double_quoted|Up], Rem, Acc, nextcol(Pos));
parse([word|Up], [$`|Rem], Acc, Pos) ->
    parse([backquoted|Up], Rem, Acc, nextcol(Pos));
parse([word|Up], [$'|Rem], Acc, Pos) ->
    parse([single_quoted|Up], Rem, Acc, nextcol(Pos));
parse([word|Up], Txt, Acc, Pos) ->
    parse([unquoted|Up], Txt, Acc, Pos);



% Comments

parse([comment|_], [], Acc, Pos) ->
    {ok, {comment, lists:reverse(Acc)}, [], Pos};
parse([comment|_], Txt = [$\n|_], Acc, Pos) ->
    {ok, {comment, lists:reverse(Acc)}, Txt, Pos};
parse(Lvls = [comment|_], Txt = [C|Rem], Acc, Pos) ->
    parse(Lvls, Rem, [C|Acc], nextcol(Pos));



% Braced strings

parse([braced|_], [], Acc, Pos) ->
    {error, missing_close_brace, Acc, Pos};
parse(Lvls = [braced|_], [${|Rem], Acc, Pos) ->
    {ok, {string, Inner}, NewRem, NewPos} =
	parse([braced|Lvls], Rem, [], nextcol(Pos)),
    parse(Lvls, NewRem, "}" ++ lists:reverse(Inner) ++ "{" ++ Acc, NewPos);
parse([braced|_], [$}|Rem], Acc, Pos) ->
    {ok, {string, lists:reverse(Acc)}, Rem, nextcol(Pos)};
parse(Lvls = [braced|_], [C=$\n|Rem], Acc, Pos) ->
    parse(Lvls, Rem, [C|Acc], nextline(Pos));
parse(Lvls = [braced|_], [$\\|[C=${|Rem]], Acc, Pos) ->
    parse(Lvls, Rem, [C|Acc], nextcol(Pos, 2));
parse(Lvls = [braced|_], [$\\|[C=$}|Rem]], Acc, Pos) ->
    parse(Lvls, Rem, [C|Acc], nextcol(Pos, 2));
parse(Lvls = [braced|_], [C|Rem], Acc, Pos) ->
    parse(Lvls, Rem, [C|Acc], nextcol(Pos));



% Double-quoted strings

parse([double_quoted|_], [], Acc, Pos) ->
    {error, missing_double_quote, Acc, Pos};
parse([double_quoted|_], [$"|Rem], Acc, Pos) ->
    {ok, {string, lists:reverse(Acc)}, Rem, nextcol(Pos)};
parse(Lvls = [double_quoted|_], [$$|Rem], Acc, Pos) ->
    {ok, VarSub, NewRem, NewPos} = parse([var_sub|Lvls], Rem, [], nextcol(Pos)),
    parse(Lvls, NewRem, [VarSub|Acc], NewPos);
parse(Lvls = [double_quoted|_], [C=$\n|Rem], Acc, Pos) ->
    parse(Lvls, Rem, [C|Acc], nextline(Pos));
parse(Lvls = [double_quoted|_], [$\\|[C=$"|Rem]], Acc, Pos) ->
    parse(Lvls, Rem, [C|Acc], nextcol(Pos, 2));
parse(Lvls = [double_quoted|_], [C|Rem], Acc, Pos) ->
    parse(Lvls, Rem, [C|Acc], nextcol(Pos));



% Backquoted strings / charlists

parse([backquoted|_], [], Acc, Pos) ->
    {error, missing_backquote, Acc, Pos};
parse([backquoted|_], [$`|Rem], Acc, Pos) ->
    {ok, {charlist, lists:reverse(Acc)}, Rem, nextcol(Pos)};
parse(Lvls = [backquoted|_], [$$|Rem], Acc, Pos) ->
    {ok, VarSub, NewRem, NewPos} = parse([var_sub|Lvls], Rem, [], nextcol(Pos)),
    parse(Lvls, NewRem, [VarSub|Acc], NewPos);
parse(Lvls = [backquoted|_], [C=$\n|Rem], Acc, Pos) ->
    parse(Lvls, Rem, [C|Acc], nextline(Pos));
parse(Lvls = [backquoted|_], [$\\|[C=$`|Rem]], Acc, Pos) ->
    parse(Lvls, Rem, [C|Acc], nextcol(Pos, 2));
parse(Lvls = [backquoted|_], [C|Rem], Acc, Pos) ->
    parse(Lvls, Rem, [C|Acc], nextcol(Pos));



% Single-quoted atoms

parse([single_quoted|_], [], Acc, Pos) ->
    {error, missing_single_quote, Acc, Pos};
parse([single_quoted|_], [$'|Rem], Acc, Pos) ->
    {ok, {atom, lists:reverse(Acc)}, Rem, nextcol(Pos)};
parse(Lvls = [single_quoted|_], [$$|Rem], Acc, Pos) ->
    {ok, VarSub, NewRem, NewPos} = parse([var_sub|Lvls], Rem, [], nextcol(Pos)),
    parse(Lvls, NewRem, [VarSub|Acc], NewPos);
parse(Lvls = [single_quoted|_], [C=$\n|Rem], Acc, Pos) ->
    parse(Lvls, Rem, [C|Acc], nextline(Pos));
parse(Lvls = [single_quoted|_], [$\\|[C=$'|Rem]], Acc, Pos) ->
    parse(Lvls, Rem, [C|Acc], nextcol(Pos, 2));
parse(Lvls = [single_quoted|_], [C|Rem], Acc, Pos) ->
    parse(Lvls, Rem, [C|Acc], nextcol(Pos));



% Unquoted/bare atoms

parse([unquoted|_], [], Acc, Pos) ->
    {ok, {atom, lists:reverse(Acc)}, [], Pos};
parse([unquoted|_], Txt = [$\s|_], Acc, Pos) ->
    {ok, {atom, lists:reverse(Acc)}, Txt, Pos};
parse(Lvls = [unquoted|_], [$$|Rem], Acc, Pos) ->
    {ok, Sub, NewRem, NewPos} = parse([var_sub|Lvls], Rem, [], nextcol(Pos)),
    parse(Lvls, NewRem, [Sub|Acc], NewPos);
parse(Lvls = [unquoted|_], [C|Rem], Acc, Pos) ->
    parse(Lvls, Rem, [C|Acc], nextcol(Pos));



% Command substitutions

parse([cmd_sub|_], [], Body, Pos) ->
    {error, missing_close_bracket, Body, Pos};
parse([cmd_sub|_], [$]|Rem], Body, Pos) ->
    {ok, {cmd_sub, Body}, Rem, nextcol(Pos);
parse(Lvls = [cmd_sub|_], Txt, [], Pos) ->
    {ok, {body, Body}, NewRem, NewPos} = parse([body|Lvls], Txt, [], Pos),
    parse(Lvls, NewRem, Body, NewPos);
% Substitution entry points
parse(Lvls = [double_quoted|_], [$[|Rem], Acc, Pos) ->
    {ok, Sub, NewRem, NewPos} = parse([cmd_sub|Lvls], Rem, [], nextcol(Pos)),
    parse(Lvls, NewRem, [Sub|Acc], NewPos);
parse(Lvls = [backquoted|_], [$[|Rem], Acc, Pos) ->
    {ok, Sub, NewRem, NewPos} = parse([cmd_sub|Lvls], Rem, [], nextcol(Pos)),
    parse(Lvls, NewRem, [Sub|Acc], NewPos);
parse(Lvls = [single_quoted|_], [$[|Rem], Acc, Pos) ->
    {ok, Sub, NewRem, NewPos} = parse([cmd_sub|Lvls], Rem, [], nextcol(Pos)),
    parse(Lvls, NewRem, [Sub|Acc], NewPos);
parse(Lvls = [unquoted|_], [$[|Rem], Acc, Pos) ->
    {ok, Sub, NewRem, NewPos} = parse([cmd_sub|Lvls], Rem, [], nextcol(Pos)),
    parse(Lvls, NewRem, [Sub|Acc], NewPos);
% Substitution escape points
parse(Lvls = [double_quoted|_], [$\\|[C=$[|Rem]], Acc, Pos) ->
    parse(Lvls, Rem, [C|Acc], nextcol(Pos, 2));
parse(Lvls = [backquoted|_], [$\\|[C=$[|Rem]], Acc, Pos) ->
    parse(Lvls, Rem, [C|Acc], nextcol(Pos, 2));
parse(Lvls = [single_quoted|_], [$\\|[C=$[|Rem]], Acc, Pos) ->
    parse(Lvls, Rem, [C|Acc], nextcol(Pos, 2));
parse(Lvls = [unquoted|_], [$\\|[C=$[|Rem]], Acc, Pos) ->
    parse(Lvls, Rem, [C|Acc], nextcol(Pos, 2));
% Substitution exit points
parse(Lvls = [body|[cmd_sub|_]], Txt = [$]|_], Acc, Pos) ->
    {ok, {body, lists:reverse(Acc)}, Txt, Pos};
parse(Lvls = [words|[body|[cmd_sub|_]]], Txt = [$]|_], Acc, Pos) ->
    {ok, {words, lists:reverse(Acc)}, Txt, Pos};
parse(Lvls = [unquoted|[body|[cmd_sub|_]]], Txt = [$]|_], Acc, Pos) ->
    {ok, {atom, lists:reverse(Acc)}, Rem, Pos};



% Variable substitutions

parse(Lvls = [var_sub|_], Txt, [], Pos) ->
    {ok, {atom, Name}, NewRem, NewPos} = parse([unquoted|Lvls], Txt, [], Pos),
    parse(Lvls, NewRem, Name, NewPos);
% Substitution entry points
parse(Lvls = [double_quoted|_], [$$|Rem], Acc, Pos) ->
    {ok, Sub, NewRem, NewPos} = parse([var_sub|Lvls], Rem, [], nextcol(Pos)),
    parse(Lvls, NewRem, [Sub|Acc], NewPos);
parse(Lvls = [backquoted|_], [$$|Rem], Acc, Pos) ->
    {ok, Sub, NewRem, NewPos} = parse([var_sub|Lvls], Rem, [], nextcol(Pos)),
    parse(Lvls, NewRem, [Sub|Acc], NewPos);
parse(Lvls = [single_quoted|_], [$$|Rem], Acc, Pos) ->
    {ok, Sub, NewRem, NewPos} = parse([var_sub|Lvls], Rem, [], nextcol(Pos)),
    parse(Lvls, NewRem, [Sub|Acc], NewPos);
parse(Lvls = [unquoted|_], [$$|Rem], Acc, Pos) ->
    {ok, Sub, NewRem, NewPos} = parse([var_sub|Lvls], Rem, [], nextcol(Pos)),
    parse(Lvls, NewRem, [Sub|Acc], NewPos);
% Substitution escape points
parse(Lvls = [double_quoted|_], [$\\|[C=$$|Rem]], Acc, Pos) ->
    parse(Lvls, Rem, [C|Acc], nextcol(Pos, 2));
parse(Lvls = [backquoted|_], [$\\|[C=$$|Rem]], Acc, Pos) ->
    parse(Lvls, Rem, [C|Acc], nextcol(Pos, 2));
parse(Lvls = [single_quoted|_], [$\\|[C=$$|Rem]], Acc, Pos) ->
    parse(Lvls, Rem, [C|Acc], nextcol(Pos, 2));
parse(Lvls = [unquoted|_], [$\\|[C=$$|Rem]], Acc, Pos) ->
    parse(Lvls, Rem, [C|Acc], nextcol(Pos, 2));
% Substitution exit points
parse([var_sub|_], [], Name, Pos) ->
    {error, missing_close_bracket, Body, Pos};
parse([var_sub|_], Txt = [$$|_], Name, Pos) ->
    {ok, {var_sub, lists:reverse(Name)}, Txt, Pos};
parse([var_sub|_], Txt = [$\s|_], Name, Pos) ->
    {ok, {var_sub, lists:reverse(Name)}, Txt, Pos};
parse([var_sub|_], Txt = [$\t|_], Name, Pos) ->
    {ok, {var_sub, lists:reverse(Name)}, Txt, Pos};
parse([var_sub|_], Txt = [$\n|_], Name, Pos) ->
    {ok, {var_sub, lists:reverse(Name)}, Txt, Pos};
parse([var_sub|_], Txt = [$;|_], Name, Pos) ->
    {ok, {var_sub, lists:reverse(Name)}, Txt, Pos};
parse(Lvls = [unquoted|[var_sub|_]], Txt = [$$|_], Acc, Pos) ->
    {ok, {atom, lists:reverse(Acc)}, Txt, Pos}.



% Helpers

initpos() -> {nofile,0,0}.

nextline(Pos) -> nextline(Pos, 1).
nextcol(Pos) -> nextcol(Pos, 1).

nextline({F,L,C}, N) -> {F,L+N,C}.

nextcol({F,L,C}, N) -> {F,L,C+N}.