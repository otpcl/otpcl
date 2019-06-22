% @doc OTPCL pipes up the wazoo.  OTPCL treats any command that starts with a
% pipe character (`|') as a command terminator - that is, it'll treat all the
% words before it as one command, then resume command parsing.  This is a bit
% tricky to describe in English, so it's easier to just show you that this:
%
% ```
% foo | bar | baz
% '''
%
% ...is equivalent to this:
%
% ```
% foo
% | bar
% | baz
% '''
%
% In this case, the `|' happens to take the return value of the
% previously-executed command (i.e. the `$RETVAL' variable) and pass it as the
% first argument to the command named in its own first argument.  That is, both
% of the above are equivalent to this:
%
% ```
% baz [bar [foo]]
% '''
%
% Of course, the command doesn't necessarily <em>have</em> to do this sort of
% chaining (or even pay attention to `$RETVAL' at all), but OTPCL's pipe support
% exists specifically to facilitate this sort of pattern, as exemplified by the
% various commands defined in this here module.
-module(otpcl_pipes).

-include("otpcl.hrl").

-export(['|!'/2, '|&'/2, '||'/2, '|*'/2, '|#'/2, '|#*'/2]).
-otpcl_cmds(['|!', '|&', '||', '|*', '|#', '|#*']).

% @doc "Send" operator.  Send the previous command's return value to the
% specified process.
'|!'([Pid], State) ->
    {RetVal, State} = otpcl_meta:get(['RETVAL'], State),
    Pid ! RetVal,
    {ok, State}.

% @doc "And Also" operator.  If the previous command returned a "truthy" value,
% run the arguments as a command.
'|&'(Args, State) ->
    alsoelse(Args, State, true, false).

% @doc "Or Else" operator.  If the previous command returned a non-"truthy"
% value, run the arguments as a command.
'||'(Args, State) ->
    alsoelse(Args, State, false, true).

alsoelse([Cmd|Args], State, Also, Else) ->
    {RetVal, State} = otpcl_meta:get(['RETVAL'], State),
    case otpcl_control:truthy(RetVal) of
        Also ->
            otpcl_meta:apply([Cmd|Args], State);
        Else ->
            {RetVal, State}
    end.

% @doc "Splat" operator.  If the previous command returned a list, and there are
% no words after the "splat", then treat the first element as a command name,
% the rest as its arguments, and run it.  Else, treat the first argument as the
% command name, pass the list elements as arguments, then pass any other passed
% arguments as additional arguments, then run the resulting command.
'|*'(Args, State) ->
    {RetVal, State} = otpcl_meta:get(['RETVAL'], State),
    splat(RetVal, Args, State).

splat([Cmd|Args], [], State) ->
    otpcl_meta:apply([Cmd|Args], State);
splat(Args, [NextCmd|NextArgs], State) ->
    NewArgs = Args ++ NextArgs,
    otpcl_meta:apply([NextCmd|NewArgs], State).

% @doc "Insert" operator.  Splits its arguments at the specified position,
% inserts the previous command's return value between them, and invokes the
% resulting list of words as a command.  That is:
%
% ```
% foo |# 0 bar baz  # -> [foo] bar baz
% foo |# 1 bar baz  # -> bar [foo] baz
% foo |# 2 bar baz  # -> bar baz [foo]
% '''
%
% Note that, for obvious reasons, trying to insert an argument into a position
% greater than the number of existing arguments will result in an error.
'|#'([Pos|Args], State) when is_integer(Pos) ->
    {RetVal, State} = otpcl_meta:get(['RETVAL'], State),
    insert_splat([RetVal], Pos, Args, State).  % Gotta stay DRY :)

insert_splat(RetVal, Pos, Args, State) when is_integer(Pos) ->
    {Front, Back} = lists:split(Pos, Args),
    otpcl_meta:apply(Front ++ RetVal ++ Back, State).

% @doc "Insert Splat" operator.  Like `|#`, but expands the previous command's
% return value during insertion (instead of just inserting the list as a single
% argument).
'|#*'([Pos|Args], State) when is_integer(Pos) ->
    {RetVal, State} = otpcl_meta:get(['RETVAL'], State),
    insert_splat(RetVal, Pos, Args, State).
