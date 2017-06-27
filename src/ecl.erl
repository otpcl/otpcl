-module(ecl).

%% TODO: export stuff

-record(variable, {name, value}).
-record(command, {name, func}).

-record(parse_item, {type, value = []}).

-record(parser, {source = <<"">>, rest = [], out = [], active = []}).

%%%%
%% MEANWHILE, IN THE command STATE...
%%%%

%% \s separates words (is dropped) in command
parse(P = #parser{ rest=[$\s | R],
                   active=[I = #parse_item{type=command} | A] }) ->
    parse(P#parser{ rest = R, active = [I] ++ A });

%% \t separates words (is dropped) in command
parse(P = #parser{ rest=[$\t | R],
                   active=[I = #parse_item{type=command} | A] }) ->
    parse(P#parser{ rest = R, active = [I] ++ A });

%% \\ + \n separates words (is dropped) in command
parse(P = #parser{ rest=[$\\ | $\n | R],  %% FIXME: no idea if this works
                   active=[I = #parse_item{type=command} | A] }) ->
    parse(P#parser{ rest = R, active = [I] ++ A });

%% # begins comment in command
parse(P = #parser{ rest=[$# | R],
                   active=[I = #parse_item{type=command} | A] }) ->
    parse(P#parser{ rest = R, active = [#parse_item{type=comment}, I] ++ A });

%% \n ends command
parse(P = #parser{ rest=[$\n | R], out=O,
                   active=[I = #parse_item{type=command} | A }) ->
    parse(P#parser{ rest = R, out = O ++ [I], active = A });

%% ; ends command
parse(P = #parser{ rest=[$; | R], out=O,
                   active=[I = #parse_item{type=command} | A }) ->
    parse(P#parser{ rest = R, out = O ++ [I], active = A });

%% ' begins single_quoted in command
parse(P = #parser{ rest=[$' | R],
                   active=[I = #parse_item{type=command} | A] }) ->
    parse(P#parser{ rest = R, active = [#parse_item{type=single_quoted}, I] ++ A });

%% " begins double_quoted in command
parse(P = #parser{ rest=[$" | R],
                   active=[I = #parse_item{type=command} | A] }) ->
    parse(P#parser{ rest = R, active = [#parse_item{type=double_quoted}, I] ++ A });

%% { begins bracketed in command
parse(P = #parser{ rest=[${ | R],
                   active=[I = #parse_item{type=command} | A] }) ->
    parse(P#parser{ rest = R, active = [#parse_item{type=bracketed}, I] ++ A });

%% [ begins cmd_sub in command
parse(P = #parser{ rest=[$[ | R],
                   active=[I = #parse_item{type=command} | A }) ->
    parse(P#parser{ rest = R, active = [#parse_item{type=cmd_sub}, I] ++ A });

%% $ begins var_sub in command
parse(P = #parser{ rest=[$$ | R],
                   active=[I = #parse_item{type=command} | A }) ->
    parse(P#parser{ rest = R, active = [#parse_item{type=var_sub}, I] ++ A });

%% \\ + C begins word with C in command
parse(P = #parser{ rest=[$\\ | C | R], %% FIXME: no idea if this works
                   active=[I = #parse_item{type=command} | A }) ->
    parse(P#parser{ rest = R, active = [#parse_item{type=word, value=[C]}, I] ++ A });

%% C begins word with C in command
parse(P = #parser{ rest=[C | R],
                   active=[I = #parse_item{type=command} | A }) ->
    parse(P#parser{ rest = R, active = [#parse_item{type=word, value=[C]}, I] ++ A });



%%%%
%% MEANWHILE, IN THE comment STATE...
%%%%

%% TODO



%%%%
%% MEANWHILE, IN THE single_quoted STATE...
%%%%

%% TODO



%%%%
%% MEANWHILE, IN THE double_quoted STATE...
%%%%

%% TODO



%%%%
%% MEANWHILE, IN THE bracketed STATE...
%%%%

%% TODO



%%%%
%% MEANWHILE, IN THE cmd_sub STATE...
%%%%

%% TODO



%%%%
%% MEANWHILE, IN THE var_sub STATE...
%%%%

%% TODO



%%%%
%% MEANWHILE, IN THE word STATE...
%%%%

%% TODO



%% Parser entry points
parse(Source) when is_binary(Source) ->
    parse(#parser{ source = Source,
                   rest = binary_to_list(Source),
                   active = [#parse_item{type=command}]}).