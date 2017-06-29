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
    lists:reverse(Acc);
parse_program(Rem, Acc) ->
    {NewRem, Cmd} = parse_command(Rem),
    parse_program(NewRem, [Cmd|Acc]).



parse_command(Txt) when is_binary(Txt) ->
    parse_command(binary_to_list(Txt));
parse_command(Txt) when is_list(Txt) ->
    parse_command(Txt, []);
parse_command(_) -> {error, invalid_input}.

parse_command([], Acc) ->
    {[], {command, lists:reverse(Acc)}};
parse_command([ $\n | Rem ], Acc) ->
    {Rem, {command, lists:reverse(Acc)}};
parse_command([ $; | Rem ], Acc) ->
    {Rem, {command, lists:reverse(Acc)}};
parse_command([ $\s | Rem ], Acc) ->
    parse_command(Rem, Acc);
parse_command([ $\t | Rem ], Acc) ->
    parse_command(Rem, Acc);
parse_command([ $\\ | [$\n | Rem] ], Acc) ->
    parse_command(Rem, Acc);
parse_command([ ${ | Rem ], Acc) ->
    {NewRem, Word} = parse_braced(Rem),
    parse_command(NewRem, [Word|Acc]);
parse_command([ $" | Rem ], Acc) ->
    {NewRem, Word} = parse_double_quoted(Rem),
    parse_command(NewRem, [Word|Acc]);
parse_command([ $' | Rem ], Acc) ->
    {NewRem, Word} = parse_single_quoted(Rem),
    parse_command(NewRem, [Word|Acc]);
parse_command(Rem, Acc) ->
    {NewRem, Word} = parse_unquoted(Rem),
    parse_command(NewRem, [Word|Acc]).



parse_braced(Txt) when is_binary(Txt) ->
    parse_braced(binary_to_list(Txt));
parse_braced(Txt) when is_list(Txt) ->
    parse_braced(Txt, []);
parse_braced(_) -> {error, invalid_input}.

parse_braced([], Acc) ->
    {[], Acc, {error, missing_close_brace}};
parse_braced([ ${ | Rem ], Acc) ->
    {NewRem, {string, Inner}} = parse_braced(Rem),
    parse_braced(NewRem, "}" ++ lists:reverse(Inner) ++ "{" ++ Acc);
parse_braced([ $} | Rem ], Acc) ->
    {Rem, {string, lists:reverse(Acc)}};
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
    {[], Acc, {error, missing_double_quote}};
parse_double_quoted([ $" | Rem ], Acc) ->
    {Rem, {string, lists:reverse(Acc)}};
parse_double_quoted([ $[ | Rem ], Acc) ->
    {NewRem, CmdSub} = parse_cmd_sub(Rem),
    parse_double_quoted(NewRem, [CmdSub|Acc]);
parse_double_quoted([ $$ | Rem ], Acc) ->
    {NewRem, VarSub} = parse_var_sub(Rem),
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
    {[], Acc, {error, missing_single_quote}};
parse_single_quoted([ $' | Rem ], Acc) ->
    {Rem, {string, lists:reverse(Acc)}};
parse_single_quoted([ $[ | Rem ], Acc) ->
    {NewRem, CmdSub} = parse_cmd_sub(Rem),
    parse_single_quoted(NewRem, [CmdSub|Acc]);
parse_single_quoted([ $$ | Rem ], Acc) ->
    {NewRem, VarSub} = parse_var_sub(Rem),
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
    {[], {string, lists:reverse(Acc)}};
parse_unquoted([ $\s | Rem ], Acc) ->
    {Rem, {string, lists:reverse(Acc)}};
parse_unquoted([ $[ | Rem ], Acc) ->
    {NewRem, CmdSub} = parse_cmd_sub(Rem),
    parse_unquoted(NewRem, [CmdSub|Acc]);
parse_unquoted([ $$ | Rem ], Acc) ->
    {NewRem, VarSub} = parse_var_sub(Rem),
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
    {[], Acc, {error, missing_close_bracket}};
parse_cmd_sub([ $[ | Rem ], Acc) ->
    {NewRem, CmdSub} = parse_cmd_sub(Rem),
    parse_cmd_sub(NewRem, [CmdSub|Acc]);
parse_cmd_sub([ $] | Rem ], Acc) ->
    {Rem, {cmd_sub, lists:reverse(Acc)}};
parse_cmd_sub([ $$ | Rem ], Acc) ->
    {NewRem, VarSub} = parse_var_sub(Rem),
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
    {[], {var_sub, lists:reverse(Acc)}};
parse_var_sub(Txt = [ $$ | _ ], Acc) ->
    {Txt, {var_sub, lists:reverse(Acc)}};
parse_var_sub(Txt = [ $[ | _ ], Acc) ->
    {Txt, {var_sub, lists:reverse(Acc)}};
parse_var_sub(Txt = [ $] | _ ], Acc) ->
    {Txt, {var_sub, lists:reverse(Acc)}};
parse_var_sub(Txt = [ $\s | _ ], Acc) ->
    {Txt, {var_sub, lists:reverse(Acc)}};
parse_var_sub([ $\\ | [C|Rem] ], Acc) ->
    parse_var_sub(Rem, [C|Acc]);
parse_var_sub([ C | Rem ], Acc) ->
    parse_var_sub(Rem, [C|Acc]).

parse_braced_var_sub(Txt) ->
    {Rem, {string, VarName}} = parse_braced(Txt),
    {Rem, {var_sub, VarName}}.
