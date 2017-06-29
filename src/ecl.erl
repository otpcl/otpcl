-module(ecl).

-export([ parse_program/1,
          parse_command/1,
          parse_braced/1,
          parse_double_quoted/1,
          parse_single_quoted/1,
          parse_unquoted/1,
          parse_cmd_sub/1,
          parse_var_sub/1 ]).


%%%%%%%%%%%%
%% Parser %%
%%%%%%%%%%%%

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

parse_command([], Acc) ->
    {ok, {command, lists:reverse(Acc)}, []};
parse_command([ $\n | Rem ], Acc) ->
    {ok, {command, lists:reverse(Acc)}, Rem};
parse_command([ $; | Rem ], Acc) ->
    {ok, {command, lists:reverse(Acc)}, Rem};
parse_command([ $\s | Rem ], Acc) ->
    parse_command(Rem, Acc);
parse_command([ $\t | Rem ], Acc) ->
    parse_command(Rem, Acc);
parse_command([ $\\ | [$\n | Rem] ], Acc) ->
    parse_command(Rem, Acc);
parse_command([ ${ | Rem ], Acc) ->
    {ok, Word, NewRem} = parse_braced(Rem),
    parse_command(NewRem, [Word|Acc]);
parse_command([ $" | Rem ], Acc) ->
    {ok, Word, NewRem} = parse_double_quoted(Rem),
    parse_command(NewRem, [Word|Acc]);
parse_command([ $' | Rem ], Acc) ->
    {ok, Word, NewRem} = parse_single_quoted(Rem),
    parse_command(NewRem, [Word|Acc]);
parse_command(Rem, Acc) ->
    {ok, Word, NewRem} = parse_unquoted(Rem),
    parse_command(NewRem, [Word|Acc]).



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



parse_single_quoted(Txt) when is_binary(Txt) ->
    parse_single_quoted(binary_to_list(Txt));
parse_single_quoted(Txt) when is_list(Txt) ->
    parse_single_quoted(Txt, []);
parse_single_quoted(_) -> {error, invalid_input}.

parse_single_quoted([], Acc) ->
    {error, missing_single_quote, Acc};
parse_single_quoted([ $' | Rem ], Acc) ->
    {ok, {string, lists:reverse(Acc)}, Rem};
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
    {ok, {string, lists:reverse(Acc)}, []};
parse_unquoted([ $\s | Rem ], Acc) ->
    {ok, {string, lists:reverse(Acc)}, Rem};
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
parse_var_sub([ $\\ | [C|Rem] ], Acc) ->
    parse_var_sub(Rem, [C|Acc]);
parse_var_sub([ C | Rem ], Acc) ->
    parse_var_sub(Rem, [C|Acc]).

parse_braced_var_sub(Txt) ->
    {ok, {string, VarName}, Rem} = parse_braced(Txt),
    {ok, {var_sub, VarName}, Rem}.


%%%%%%%%%%%%%%%%%
%% Interpreter %%
%%%%%%%%%%%%%%%%%

interpret(_) ->
    ok.
