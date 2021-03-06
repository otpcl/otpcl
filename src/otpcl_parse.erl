% @doc OTPCL's parser.
%
% Unlike most Erlang-derived languages, OTPCL's parser is not based on
% leex/yecc; rather, it's written by hand (as a side note: the author has no
% idea exactly what sort of parser OTPCL's parser actually is, though "recursive
% descent" sounds approximately right, given that it's recursive and it
% descends; someone who actually went to college is welcome to try to make sense
% of this module and provide a better explanation of what sort of parser it
% implements).  The parser is (as far as the author can surmise) linear and
% relatively efficient, albeit only because it "cheats" by punting some things
% to the interpreter (notably: the parser treats numbers as atoms, so the
% interpreter is required to reparse atoms if it wants to be able to interpret
% them as numbers).
%
% == Syntax ==
%
% A "free" character is a character that is neither escaped (i.e. immediately
% preceded by a backslash character, provided that backslash character is itself
% "free") nor already part of a lower-level construct.
%
% A program is a list of statements separated by contiguous sequences of free
% vertical whitespace characters or semicolons.
%
% A statement is a list of words separated by contiguous sequences of free
% horizontal whitespace characters (escaped vertical whitespace characters are
% considered to be horizontal whitespace characters).  Statements may be treated
% as "commands" in certain contexts (e.g. commands are specifically the
% top-level children of a program).
%
% A word is a braced string, double-quoted string, backquoted charlist,
% single-quoted atom, braced variable, unquoted variable, function call, list,
% tuple, comment, pipe, or unquoted atom.
%
% A braced string is a free opening curly brace, followed by zero or more
% characters and/or braced strings, followed by a free closing curly brace.
% That is: a braced string can be inside a braced string (and curly braces not
% intended to begin/end an inner braced string should be escaped with an
% immediately-preceding backslash).
%
% A double-quoted string is a free double-quote, followed by zero or more
% characters, followed by a free double-quote.
%
% A backquoted charlist is a free backquote, followed by zero or more
% characters, followed by a free backquote.
%
% A single-quoted atom is a free single-quote, followed by zero or more
% characters, followed by a free single-quote.
%
% A braced variable is a free dollar-sign, followed by a braced string.
%
% An unquoted variable is a free dollar-sign, followed by a contiguous sequence
% of characters, terminated by the next free whitespace, semicolon, or (when
% expected by the parser) closing parenthesis, square bracket, angle bracket, or
% curly brace.  Unquoted variables may not contain free opening parentheses,
% square brackets, angle brackets, or curly braces; if encountered, the parser
% will immediately return an error (this may change in the future).
%
% A function call is a free opening square bracket, followed by a statement,
% followed by a free closing square bracket.  It is currently an error for a
% function call to contain more or less than one statement (this may change in
% the future).
%
% A list is a free opening parenthesis, followed by a statement (note: the
% statement is treated purely as a list of words), followed by a free closing
% parenthesis.  It is currently an error for a list to contain more than one
% statement (this will change in the future).
%
% A tuple is a free opening angle bracket, followed by a statement (note: the
% statement is treated purely as a list of words), followed by a free closing
% angle bracket.  It is currently an error for a tuple to contain more than one
% statement (this will change in the future).
%
% A comment is a free octothorpe, followed by a contiguous sequence of
% characters, terminated by the next vertical whitespace character.  A comment
% terminates the statement in which it is encountered.
%
% A pipe is a free pipe character, followed optionally by a contiguous sequence
% of characters, terminated by the next free whitespace.  The pipe itself is
% parsed as an unquoted atom, which becomes the first word in a new statement.
%
% An unquoted atom is a contiguous sequence of characters, terminated by the
% next free whitespace, semicolon, or (when expected by the parser) closing
% parenthesis, square bracket, angle bracket, or curly brace.  Unquoted atoms
% may not contain free opening parentheses, square brackets, angle brackets, or
% curly braces; if encountered, the parser will immediately return an error
% (this may change in the future).
%
% == Output ==
%
% OTPCL's parser does not emit the same exact structures as Erlang's parser
% (that is: it does not generate Erlang-compatible parse trees).  This was
% probably a mistake (and may very well change, notably because it'd presumably
% make OTPCL compilation easier by just piggybacking on the existing
% Erlang-oriented infrastructure), but it works well enough for now.
%
% === Tokens ===
%
% The lexer makes no attempt to actually classify different types of characters
% (unlike Erlang's lexer); thus, each "token" is simply `{Char, Pos={F,L,C}}',
% where `Char' is a character code point and `Pos' is the position of that
% character (that is, `Char' came from column `C' of line `L' of file
% `F').
%
% === Trees ===
%
% The syntax tree the parser emits is a recursive 3-element tuple of the form
% `{parsed, Type, Branches}', where `Type' is an atom and `Branches' is a list
% of either tokens or trees.  By default (i.e. when calling parse/1), the root
% of the tree will be a `program', with `command' and/or `comment' branches
% (`pipe's are also parsed at this level, but the parser converts those to
% `command's).
-module(otpcl_parse).

-include("otpcl.hrl").

-export([scan/1, scan/2, parse/1, parse/2, initpos/0, initpos/1]).

-ifdef(DEBUG).
-define(DEBUG_PRINT(Msg, Args), io:format(Msg, Args)).
-else.
-define(DEBUG_PRINT(Msg, Args), ok).
-endif.


%% I don't know if this really counts as "lexing", but it does
%% associate every character with a line/column, which means parse/3
%% doesn't need to care about it.  Probably not memory-optimal,
%% though.

-spec scan(str_or_bin()) -> [token()].
% @doc Converts a string into a list of tokens.
scan(Txt) ->
    scan(Txt, [], initpos()).

-spec scan(str_or_bin(), position()) -> [token()].
% @doc Converts a string into a list of tokens, starting at the specified
% position.
scan(Txt, Pos) ->
    scan(Txt, [], Pos).

-spec scan(str_or_bin(), [token()], position()) -> [token()].
scan(Txt, Acc, Pos) when is_binary(Txt) ->
    scan(binary_to_list(Txt), Acc, Pos);
scan([], Acc, _) ->
    lists:reverse(Acc);
scan([Char|Rem], Acc, Pos) ->
    scan(Rem, [{Char, Pos}|Acc], nextpos(Char, Pos)).


%% This is where the fun begins

-spec parse(str_or_bin()) -> parse_success() | parse_error().
% @doc Like parse/2, but defaulting to `program' as the toplevel parse tree
% element.
parse(Input) ->
    parse([program], Input).

-spec parse([level(),...], str_or_bin()) -> parse_success() | parse_error().
% @doc Attempts to parse either a string or token list.  Returns either a
% success response `{ok, Tree, Rem}' (where `Tree' is an OTPCL parse tree and
% `Rem' is whatever characters were left over
parse(Lvls, Input) ->
    case is_text(Input) of
        true -> parse(Lvls, scan(Input), []);
        _    -> parse(Lvls, Input, [])
    end.


%% I ain't got the fancy schmancy college edumacation to know the
%% *right* terminology for how parsers/lexers work, but here's the
%% terminology I'm using for the conventions below:
%%
%% TOKEN:   do something upon matching a specific token
%% TPAIR:   do something upon matching a specific pair of tokens
%% ANY:     do something upon matching any token
%% EOF:     do something upon running out of input tokens
%%
%% DROP:    don't store the next token anywhere
%% TAKE:    put the token in the current level's accumulator
%% KEEP:    put the token back into the current level's remainder
%% EXIT:    stop parsing the current level and either...
%%   OK:    ...return the resulting node or...
%%   ERROR: ...return an error with the reason and accumulator
%%
%% SWITCH:  replace the current level (useful for dispatching node
%%          subtypes)
%% DESCEND: start a new level, then stick the resulting node in the
%%          current level's accumulator
%% FLATTEN: append the current level's grandchildren (plus the
%%          specified characters at the front and end) to its
%%          accumulator
%% ESCAPED: the current token is an unescaped backslash, so drop it
%%          and use the next token instead (usually for a TAKE)

-define(TOKEN_DROP(Lvl, Char),
        parse(Lvls = [Lvl|_], [{Char,_}|Rem], Acc) ->
               ?DEBUG_PRINT("~p: dropping token ~p\n", [Lvl, Char]),
               parse(Lvls, Rem, Acc)).

-define(TPAIR_DROP(Lvl, First, Second),
        parse(Lvls = [Lvl|_], [{First,_}|[{Second,_}|Rem]], Acc) ->
               ?DEBUG_PRINT("~p: dropping token pair ~p/~p\n",
                            [Lvl, First, Second]),
               parse(Lvls, Rem, Acc)).

-define(TOKEN_DROP_SWITCH(Old, Char, New),
        parse([Old|Up], [{Char,_}|Rem], Acc) ->
               ?DEBUG_PRINT("~p: dropping token ~p and switching to ~p\n",
                            [Old, Char, New]),
               parse([New|Up], Rem, Acc)).

-define(TOKEN_TAKE_DESCEND(Lvl, Char, SubLvl),
        parse(Lvls = [Lvl|_], [{Char,_}|Rem], Acc) ->
               ?DEBUG_PRINT("~p: taking token ~p and descending to ~p\n",
                            [Lvl, Char, SubLvl]),
               case parse([SubLvl|Lvls], Rem) of
                   {ok, Child, NewRem} ->
                       parse(Lvls, NewRem, [Child|Acc]);
                   Error ->
                       Error
               end).

-define(TOKEN_KEEP_DESCEND(Lvl, Char, SubLvl),
        parse(Lvls = [Lvl|_], Tokens = [{Char,_}|_], Acc) ->
               ?DEBUG_PRINT("~p: leaving token ~p for descent to ~p\n",
                            [Lvl, Char, SubLvl]),
               case parse([SubLvl|Lvls], Tokens) of
                   {ok, Child, NewRem} ->
                       parse(Lvls, NewRem, [Child|Acc]);
                   Error ->
                       Error
               end).

-define(TOKEN_TAKE_ESCAPED(Lvl, Char),
        parse(Lvls = [Lvl|_], [$\\|[T={Char,_}|Rem]], Acc) ->
               ?DEBUG_PRINT("~p: taking escaped token ~p\n", [Lvl, Char]),
               parse(Lvls, Rem, [T|Acc])).

-define(TOKEN_TAKE_DESCEND_FLATTEN(Lvl, Start, SubLvl, End),
        parse(Lvls = [Lvl|_], [SChar = {Start,_}|Rem], Acc) ->
               ?DEBUG_PRINT("~p: taking token ~p for flat descent into ~p "
                            ++ "with ending token ~p\n",
                            [Lvl, Start, SubLvl, End]),
               case parse([SubLvl|Lvls], Rem) of
                   {ok, {parsed,_,Inner}, NewRem} ->
                       case NewRem of
                           [{_,RemPos}|_] ->
                               EChar = {End,nextpos(End,RemPos)},
                               NewAcc = [EChar] ++ lists:reverse(Inner)
                                   ++ [SChar] ++ Acc,
                               parse(Lvls, NewRem, NewAcc);
                           [] ->
                               {error, {expected, End}, Lvl, NewRem,
                                lists:reverse(Inner) ++ [SChar] ++ Acc}
                       end;
                   Error ->
                       Error
               end).

-define(TOKEN_EXIT_OK(Lvl, Char),
        parse([Lvl|_], [{Char,_}|Rem], Acc) ->
               ?DEBUG_PRINT("~p: token ~p is a valid exit point\n",
                            [Lvl, Char]),
               {ok, {parsed, Lvl, lists:reverse(Acc)}, Rem}).

-define(TOKEN_EXIT_ERROR(Lvl, Char, Reason),
        parse([Lvl|_], Rem = [{Char,_}|_], Acc) ->
               ?DEBUG_PRINT("~p: token ~p is an invalid exit point\n",
                            [Lvl, Char]),
               {error, Reason, Lvl, Rem, Acc}).

-define(TOKEN_KEEP_EXIT_OK(Lvl, Char),
        parse([Lvl|_], Tokens = [{Char,_}|_], Acc) ->
               ?DEBUG_PRINT("~p: token ~p is a valid exit point; leaving for "
                            ++ "parent\n", [Lvl, Char]),
               {ok, {parsed, Lvl, lists:reverse(Acc)}, Tokens}).

-define(TPAIR_EXIT_OK(Lvl, First, Second),
        parse([Lvl|_], [{First,_}|[{Second,_}|Rem]], Acc) ->
               ?DEBUG_PRINT("~p: token pair ~p/~p is a valid exit point\n",
                            [Lvl, First, Second]),
               {ok, {parsed, Lvl, lists:reverse(Acc)}, Rem}).

-define(TPAIR_KEEP_EXIT_OK(Lvl, First, Second),
        parse([Lvl|_], Tokens = [{First,_}|[{Second,_}|_]], Acc) ->
               ?DEBUG_PRINT("~p: token pair ~p/~p is a valid exit point; "
                            ++ "leaving for parent\n", [Lvl, First, Second]),
               {ok, {parsed, Lvl, lists:reverse(Acc)}, Tokens}).

-define(TPAIR_DROP_SWITCH(Old, First, Second, New),
        parse([Old|Up], [{First,_}|[{Second,_}|Rem]], Acc) ->
               ?DEBUG_PRINT("~p: dropping token pair ~p/~p and switching to "
                            ++ "~p\n", [Old, First, Second, New]),
               parse([New|Up], Rem, Acc)).

-define(EOF_EXIT_OK(Lvl),
        parse([Lvl|_], [], Acc) ->
               ?DEBUG_PRINT("~p: EOF is a valid exit point\n", [Lvl]),
               {ok, {parsed, Lvl, lists:reverse(Acc)}, []}).

-define(EOF_EXIT_ERROR(Lvl, Reason),
        parse([Lvl|_], [], Acc) ->
               ?DEBUG_PRINT("~p: EOF is an invalid exit point\n", [Lvl]),
               {error, Reason, Lvl, [], Acc}).

-define(ANY_KEEP_DESCEND(Lvl, SubLvl),
        parse(Lvls = [Lvl|_], Tokens, Acc) ->
               ?DEBUG_PRINT("~p: unconditionally descending into ~p\n",
                            [Lvl, SubLvl]),
               case parse([SubLvl|Lvls], Tokens) of
                   {ok, Child, Rem} ->
                       parse(Lvls, Rem, [Child|Acc]);
                   Error ->
                       Error
               end).

-define(ANY_KEEP_SWITCH(Old, New),
        parse([Old|Up], Tokens, Acc) ->
               ?DEBUG_PRINT("~p: unconditionally switching into ~p\n",
                            [Old, New]),
               parse([New|Up], Tokens, Acc)).

-define(ANY_TAKE(Lvl),
        parse(Lvls = [Lvl|_], [T|Rem], Acc) ->
               ?DEBUG_PRINT("~p: unconditionally taking token ~p\n", [Lvl,T]),
               parse(Lvls, Rem, [T|Acc])).

-spec parse([level(),...], [token()], [tree()] | [token()]) ->
                   parse_success() | parse_error().

?EOF_EXIT_OK(program);
?TOKEN_TAKE_DESCEND(program, $#, comment);
?TOKEN_KEEP_DESCEND(program, $|, pipe);
?TOKEN_DROP(program, $\s);
?TOKEN_DROP(program, $\t);
?ANY_KEEP_DESCEND(program, command);

?EOF_EXIT_OK(command);
?TOKEN_EXIT_OK(command, $\n);
?TOKEN_EXIT_OK(command, $;);
?TOKEN_KEEP_EXIT_OK(command, $#);
?TOKEN_KEEP_EXIT_OK(command, $|);
?TPAIR_DROP(command, $\\, $\n);
?TPAIR_DROP(command, $\\, $;);
?TOKEN_DROP(command, $\s);
?TOKEN_DROP(command, $\t);
?ANY_KEEP_DESCEND(command, word);

?TOKEN_EXIT_OK(word, $\s);
?TOKEN_EXIT_OK(word, $\t);
?TOKEN_EXIT_OK(word, $\n);
?TOKEN_EXIT_OK(word, $;);
?TOKEN_KEEP_EXIT_OK(word, $#);
?TOKEN_KEEP_EXIT_OK(word, $|);
?TOKEN_DROP_SWITCH(word, ${, braced);
?TOKEN_EXIT_ERROR(word, $}, {unexpected, $}});
?TOKEN_DROP_SWITCH(word, $", double_quoted);
?TOKEN_DROP_SWITCH(word, $`, backquoted);
?TOKEN_DROP_SWITCH(word, $', single_quoted);
?TPAIR_DROP_SWITCH(word, $$, ${, var_braced);
?TOKEN_DROP_SWITCH(word, $$, var_unquoted);
?TOKEN_DROP_SWITCH(word, $[, funcall);
?TOKEN_EXIT_ERROR(word, $], {unexpected, $]});
?TOKEN_DROP_SWITCH(word, $(, list);
?TOKEN_EXIT_ERROR(word, $), {unexpected, $)});
?TOKEN_DROP_SWITCH(word, $<, tuple);
?TOKEN_EXIT_ERROR(word, $>, {unexpected, $>});
?ANY_KEEP_SWITCH(word, unquoted);

?EOF_EXIT_OK(comment);
?TOKEN_EXIT_OK(comment, $\n);
?ANY_TAKE(comment);

?EOF_EXIT_OK(pipe);
?TOKEN_KEEP_DESCEND(pipe, $|, unquoted);
?ANY_KEEP_SWITCH(pipe, command);

?EOF_EXIT_ERROR(braced, {expected, $}});
?TOKEN_EXIT_OK(braced, $});
?TOKEN_TAKE_DESCEND_FLATTEN(braced, ${, braced, $});
?TOKEN_TAKE_ESCAPED(braced, ${);
?TOKEN_TAKE_ESCAPED(braced, $});
?TOKEN_TAKE_ESCAPED(braced, $\\);
?ANY_TAKE(braced);

?EOF_EXIT_ERROR(double_quoted, {expected, $"});
?TOKEN_EXIT_OK(double_quoted, $");
?TOKEN_TAKE_ESCAPED(double_quoted, $");
?TOKEN_TAKE_ESCAPED(double_quoted, $\\);
?ANY_TAKE(double_quoted);

?EOF_EXIT_ERROR(backquoted, {expected, $`});
?TOKEN_EXIT_OK(backquoted, $`);
?TOKEN_TAKE_ESCAPED(backquoted, $`);
?TOKEN_TAKE_ESCAPED(backquoted, $\\);
?ANY_TAKE(backquoted);

?EOF_EXIT_ERROR(single_quoted, {expected, $'});
?TOKEN_EXIT_OK(single_quoted, $');
?TOKEN_TAKE_ESCAPED(single_quoted, $');
?TOKEN_TAKE_ESCAPED(single_quoted, $\\);
?ANY_TAKE(single_quoted);

?EOF_EXIT_OK(unquoted);
?TOKEN_EXIT_OK(unquoted, $\s);
?TOKEN_EXIT_OK(unquoted, $\t);
?TOKEN_KEEP_EXIT_OK(unquoted, $\n);
?TPAIR_KEEP_EXIT_OK(unquoted, $\\, $\n);
?TOKEN_KEEP_EXIT_OK(unquoted, $;);
?TOKEN_KEEP_EXIT_OK(unquoted, $]);
?TOKEN_KEEP_EXIT_OK(unquoted, $));
?TOKEN_KEEP_EXIT_OK(unquoted, $>);
?TOKEN_EXIT_ERROR(unquoted, $[, {unexpected, $[});
?TOKEN_EXIT_ERROR(unquoted, $(, {unexpected, $(});
?TOKEN_EXIT_ERROR(unquoted, $<, {unexpected, $<});
?TOKEN_TAKE_ESCAPED(unquoted, $\s);
?TOKEN_TAKE_ESCAPED(unquoted, $\t);
?TOKEN_TAKE_ESCAPED(unquoted, $\\);
?TOKEN_TAKE_ESCAPED(unquoted, $;);
?TOKEN_TAKE_ESCAPED(unquoted, $[);
?TOKEN_TAKE_ESCAPED(unquoted, $]);
?TOKEN_TAKE_ESCAPED(unquoted, $();
?TOKEN_TAKE_ESCAPED(unquoted, $));
?TOKEN_TAKE_ESCAPED(unquoted, $<);
?TOKEN_TAKE_ESCAPED(unquoted, $>);
?ANY_TAKE(unquoted);

?EOF_EXIT_OK(var_unquoted);
?TOKEN_EXIT_OK(var_unquoted, $\s);
?TOKEN_EXIT_OK(var_unquoted, $\t);
?TOKEN_KEEP_EXIT_OK(var_unquoted, $\n);
?TPAIR_KEEP_EXIT_OK(var_unquoted, $\\, $\n);
?TOKEN_KEEP_EXIT_OK(var_unquoted, $;);
?TOKEN_KEEP_EXIT_OK(var_unquoted, $]);
?TOKEN_KEEP_EXIT_OK(var_unquoted, $));
?TOKEN_KEEP_EXIT_OK(var_unquoted, $>);
?TOKEN_EXIT_ERROR(var_unquoted, $[, {unexpected, $[});
?TOKEN_EXIT_ERROR(var_unquoted, $(, {unexpected, $(});
?TOKEN_EXIT_ERROR(var_unquoted, $<, {unexpected, $<});
?TOKEN_TAKE_ESCAPED(var_unquoted, $\s);
?TOKEN_TAKE_ESCAPED(var_unquoted, $\t);
?TOKEN_TAKE_ESCAPED(var_unquoted, $\\);
?TOKEN_TAKE_ESCAPED(var_unquoted, $;);
?TOKEN_TAKE_ESCAPED(var_unquoted, $[);
?TOKEN_TAKE_ESCAPED(var_unquoted, $]);
?TOKEN_TAKE_ESCAPED(var_unquoted, $();
?TOKEN_TAKE_ESCAPED(var_unquoted, $));
?TOKEN_TAKE_ESCAPED(var_unquoted, $<);
?TOKEN_TAKE_ESCAPED(var_unquoted, $>);
?ANY_TAKE(var_unquoted);

?EOF_EXIT_ERROR(var_braced, {expected, $'});
?TOKEN_EXIT_OK(var_braced, $});
?TOKEN_TAKE_DESCEND_FLATTEN(var_braced, ${, braced, $});
?TOKEN_TAKE_ESCAPED(var_braced, ${);
?TOKEN_TAKE_ESCAPED(var_braced, $});
?TOKEN_TAKE_ESCAPED(var_braced, $\\);
?ANY_TAKE(var_braced);

?EOF_EXIT_ERROR(funcall, {expected, $]});
?TOKEN_EXIT_OK(funcall, $]);
?TOKEN_EXIT_ERROR(funcall, $\n, {unimplemented, progcalls});
?TPAIR_DROP(funcall, $\\, $\n);
?TOKEN_DROP(funcall, $\s);
?TOKEN_DROP(funcall, $\t);
?ANY_KEEP_DESCEND(funcall, word);

?EOF_EXIT_ERROR(list, {expected, $)});
?TOKEN_EXIT_OK(list, $));
?TOKEN_EXIT_ERROR(list, $\n, {unimplemented, tables});
?TPAIR_DROP(list, $\\, $\n);
?TOKEN_DROP(list, $\s);
?TOKEN_DROP(list, $\t);
?ANY_KEEP_DESCEND(list, word);

?EOF_EXIT_ERROR(tuple, {expected, $>});
?TOKEN_EXIT_OK(tuple, $>);
?TOKEN_EXIT_ERROR(tuple, $\n, {unimplemented, matrices});
?TPAIR_DROP(tuple, $\\, $\n);
?TOKEN_DROP(tuple, $\s);
?TOKEN_DROP(tuple, $\t);
?ANY_KEEP_DESCEND(tuple, word);

parse(Lvls, Rem, Acc) ->
    {error, {unexpected, other}, Lvls, Rem, Acc}.

-spec initpos() -> position().
% @doc Column 0 of row 0 of file `nofile'.
initpos() ->
    {nofile,0,0}.

-spec initpos(any()) -> position().
% @doc Column 0 of row 0 of file `File'.
initpos(Filename) ->
    {Filename,0,0}.

-spec nextpos(char(), position()) -> position().
% @doc Increments the row and sets the column to 0 for vertical whitespace
% characters; else, increments the column only.
nextpos($\n, {F,L,_}) ->
    {F,L+1,0};
nextpos(_, {F,L,C}) ->
    {F,L,C+1}.

-spec is_text(str_or_bin()) -> boolean().
% @doc True if character list or binary; false otherwise.
is_text(Txt) ->
    is_binary(Txt) or io_lib:char_list(Txt).
