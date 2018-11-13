-module(otpcl_parse).

-export([scan/1, parse/1]).

-ifdef(DEBUG).
-define(DEBUG_PRINT(Msg, Args), io:format(Msg, Args)).
-else.
-define(DEBUG_PRINT(Msg, Args), ok).
-endif.


%% I don't know if this really counts as "lexing", but it does
%% associate every character with a line/column, which means parse/3
%% doesn't need to care about it.  Probably not memory-optimal,
%% though.

scan(Txt) ->
    scan(Txt, initpos()).
scan(Txt, Pos) ->
    scan(Txt, [], Pos).

scan(Txt, Acc, Pos) when is_binary(Txt) ->
    scan(binary_to_list(Txt), Acc, Pos);
scan([], Acc, _) ->
    lists:reverse(Acc);
scan([Char|Rem], Acc, Pos) ->
    scan(Rem, [{Char, Pos}|Acc], nextpos(Char, Pos)).


%% This is where the fun begins

parse(Txt) ->
    case is_text(Txt) of
        true -> parse(scan(Txt));
        _    -> parse([program], Txt)
    end.

parse(Lvls, Tokens) ->
    parse(Lvls, Tokens, []).


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
               {ok, Child, NewRem} = parse([SubLvl|Lvls], Rem),
               parse(Lvls, NewRem, [Child|Acc])).

-define(TOKEN_TAKE_ESCAPED(Lvl, Char),
        parse(Lvls = [Lvl|_], [$\\|[T={Char,_}|Rem]], Acc) ->
               ?DEBUG_PRINT("~p: taking escaped token ~p\n", [Lvl, Char]),
               parse(Lvls, Rem, [T|Acc])).

-define(TOKEN_TAKE_DESCEND_FLATTEN(Lvl, Start, SubLvl, End),
        parse(Lvls = [Lvl|_], [SChar = {Start,_}|Rem], Acc) ->
               ?DEBUG_PRINT("~p: taking token ~p for flat descent into ~p "
                            ++ "with ending token ~p\n",
                            [Lvl, Start, SubLvl, End]),
               {ok, {parsed, _, Inner}, NewRem} = parse([SubLvl|Lvls], Rem),
               case NewRem of
                   [{_,RemPos}|_] ->
                       EChar = {End,nextpos(End,RemPos)},
                       NewAcc = [EChar] ++ lists:reverse(Inner) ++ [SChar]
                           ++ Acc,
                       parse(Lvls, NewRem, NewAcc);
                   [] -> {error, missing_close_brace, lists:reverse(Inner)
                          ++ [SChar] ++ Acc}
               end).

-define(TOKEN_EXIT_OK(Lvl, Char),
        parse([Lvl|_], [{Char,_}|Rem], Acc) ->
               ?DEBUG_PRINT("~p: token ~p is a valid exit point\n",
                            [Lvl, Char]),
               {ok, {parsed, Lvl, lists:reverse(Acc)}, Rem}).

-define(TOKEN_EXIT_ERROR(Lvl, Char, Reason),
        parse([Lvl|_], [{Char,_}|_], Acc) ->
               ?DEBUG_PRINT("~p: token ~p is an invalid exit point\n",
                            [Lvl, Char]),
               {error, Reason, Acc}).

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
        parse([Old|Up], [First|[Second|Rem]], Acc) ->
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
               {error, Reason, Acc}).

-define(ANY_KEEP_DESCEND(Lvl, SubLvl),
        parse(Lvls = [Lvl|_], Tokens, Acc) ->
               ?DEBUG_PRINT("~p: unconditionally descending into ~p\n",
                            [Lvl, SubLvl]),
               {ok, Child, Rem} = parse([SubLvl|Lvls], Tokens),
               parse(Lvls, Rem, [Child|Acc])).

-define(ANY_KEEP_SWITCH(Old, New),
        parse([Old|Up], Tokens, Acc) ->
               ?DEBUG_PRINT("~p: unconditionally switching into ~p\n",
                            [Old, New]),
               parse([New|Up], Tokens, Acc)).

-define(ANY_TAKE(Lvl),
        parse(Lvls = [Lvl|_], [T|Rem], Acc) ->
               ?DEBUG_PRINT("~p: unconditionally taking token ~p\n", [Lvl,T]),
               parse(Lvls, Rem, [T|Acc])).


?EOF_EXIT_OK(program);
?ANY_KEEP_DESCEND(program, command);

?EOF_EXIT_OK(command);
?TOKEN_EXIT_OK(command, $\n);
?TPAIR_DROP(command, $\\, $\n);
?TOKEN_DROP(command, $\s);
?TOKEN_DROP(command, $\t);
?ANY_KEEP_DESCEND(command, word);

?TOKEN_EXIT_OK(word, $\s);
?TOKEN_EXIT_OK(word, $\t);
?TOKEN_EXIT_OK(word, $\n);
?TOKEN_DROP_SWITCH(word, $#, comment);
?TOKEN_DROP_SWITCH(word, ${, braced);
?TOKEN_EXIT_ERROR(word, $}, unexpected_close_brace);
?TOKEN_DROP_SWITCH(word, $", double_quoted);
?TOKEN_DROP_SWITCH(word, $`, backquoted);
?TOKEN_DROP_SWITCH(word, $', single_quoted);
?TPAIR_DROP_SWITCH(word, $$, ${, var_braced);
?TOKEN_DROP_SWITCH(word, $$, var_unquoted);
?TOKEN_DROP_SWITCH(word, $[, funcall);
?TOKEN_EXIT_ERROR(word, $], unexpected_close_bracket);
?TOKEN_DROP_SWITCH(word, $(, list);
?TOKEN_EXIT_ERROR(word, $), unexpected_close_paren);
?TOKEN_DROP_SWITCH(word, $<, tuple);
?TOKEN_EXIT_ERROR(word, $>, unexpected_close_angle_bracket);
?ANY_KEEP_SWITCH(word, unquoted);

?EOF_EXIT_OK(comment);
?TOKEN_EXIT_OK(comment, $\n);
?ANY_TAKE(comment);

?EOF_EXIT_ERROR(braced, missing_close_brace);
?TOKEN_EXIT_OK(braced, $});
?TOKEN_TAKE_DESCEND_FLATTEN(braced, ${, braced, $});
?TOKEN_TAKE_ESCAPED(braced, ${);
?TOKEN_TAKE_ESCAPED(braced, $});
?TOKEN_TAKE_ESCAPED(braced, $\\);
?ANY_TAKE(braced);

?EOF_EXIT_ERROR(double_quoted, missing_double_quote);
?TOKEN_EXIT_OK(double_quoted, $");
?TOKEN_TAKE_ESCAPED(double_quoted, $");
?TOKEN_TAKE_ESCAPED(double_quoted, $\\);
?ANY_TAKE(double_quoted);

?EOF_EXIT_ERROR(backquoted, missing_backquote);
?TOKEN_EXIT_OK(backquoted, $`);
?TOKEN_TAKE_ESCAPED(backquoted, $`);
?TOKEN_TAKE_ESCAPED(backquoted, $\\);
?ANY_TAKE(backquoted);

?EOF_EXIT_ERROR(single_quoted, missing_single_quote);
?TOKEN_EXIT_OK(single_quoted, $');
?TOKEN_TAKE_ESCAPED(single_quoted, $');
?TOKEN_TAKE_ESCAPED(single_quoted, $\\);
?ANY_TAKE(single_quoted);

?EOF_EXIT_OK(unquoted);
?TOKEN_EXIT_OK(unquoted, $\s);
?TOKEN_EXIT_OK(unquoted, $\t);
?TOKEN_KEEP_EXIT_OK(unquoted, $\n);
?TPAIR_KEEP_EXIT_OK(unquoted, $\\, $\n);
?TOKEN_KEEP_EXIT_OK(unquoted, $]);
?TOKEN_KEEP_EXIT_OK(unquoted, $));
?TOKEN_KEEP_EXIT_OK(unquoted, $>);
?TOKEN_TAKE_ESCAPED(unquoted, $\s);
?TOKEN_TAKE_ESCAPED(unquoted, $\t);
?TOKEN_TAKE_ESCAPED(unquoted, $\\);
?ANY_TAKE(unquoted);

?EOF_EXIT_OK(var_unquoted);
?TOKEN_EXIT_OK(var_unquoted, $\s);
?TOKEN_EXIT_OK(var_unquoted, $\t);
?TOKEN_KEEP_EXIT_OK(var_unquoted, $\n);
?TPAIR_KEEP_EXIT_OK(var_unquoted, $\\, $\n);
?TOKEN_KEEP_EXIT_OK(var_unquoted, $]);
?TOKEN_KEEP_EXIT_OK(var_unquoted, $));
?TOKEN_KEEP_EXIT_OK(var_unquoted, $>);
?TOKEN_TAKE_ESCAPED(var_unquoted, $\s);
?TOKEN_TAKE_ESCAPED(var_unquoted, $\t);
?TOKEN_TAKE_ESCAPED(var_unquoted, $\\);
?ANY_TAKE(var_unquoted);

?EOF_EXIT_ERROR(var_braced, missing_close_brace);
?TOKEN_EXIT_OK(var_braced, $});
?TOKEN_TAKE_DESCEND_FLATTEN(var_braced, ${, braced, $});
?TOKEN_TAKE_ESCAPED(var_braced, ${);
?TOKEN_TAKE_ESCAPED(var_braced, $});
?TOKEN_TAKE_ESCAPED(var_braced, $\\);
?ANY_TAKE(var_braced);

?EOF_EXIT_ERROR(funcall, missing_close_bracket);
?TOKEN_EXIT_OK(funcall, $]);
?TPAIR_DROP(funcall, $\\, $\n);
?TOKEN_DROP(funcall, $\s);
?TOKEN_DROP(funcall, $\t);
?ANY_KEEP_DESCEND(funcall, word);

?EOF_EXIT_ERROR(list, missing_close_paren);
?TOKEN_EXIT_OK(list, $));
?TPAIR_DROP(list, $\\, $\n);
?TOKEN_DROP(list, $\s);
?TOKEN_DROP(list, $\t);
?ANY_KEEP_DESCEND(list, word);

?EOF_EXIT_ERROR(tuple, missing_close_angle_bracket);
?TOKEN_EXIT_OK(tuple, $>);
?TPAIR_DROP(tuple, $\\, $\n);
?TOKEN_DROP(tuple, $\s);
?TOKEN_DROP(tuple, $\t);
?ANY_KEEP_DESCEND(tuple, word);

parse(Lvls, Rem, Acc) ->
    {error, unexpected_token, Lvls, Rem, Acc}.


initpos() ->
    {nofile,0,0}.

nextpos($\n, {F,L,_}) ->
    {F,L+1,0};
nextpos(_, {F,L,C}) ->
    {F,L,C+1}.

is_text(Txt) ->
    is_binary(Txt) or io_lib:char_list(Txt).
