% @doc Core OTPCL commands.  These commands are technically optional, but
% leaving them out (i.e. in a custom interpreter state) can create some rather
% peculiar results, in particular since this is where basic commands like
% `return' and `|' live (yes, the OTPCL standard pipe is internally an ordinary
% command, so you can call it on its own; you can also define your own
% pipe-commands, as detailed below).
-module(otpcl_core).

-include("otpcl.hrl").

-export(['CMD_return'/2, return/2, 'CMD_|'/2, '|'/2]).

'CMD_return'(Args, State) ->
    return(Args, State).
'CMD_|'(Args, State) ->
    '|'(Args, State).

-spec return(any(), state()) -> {any(), state()}.
% @doc Sets `$RETVAL' in the given state.  `$RETVAL' will be set to one of `ok'
% (if passed an empty list), the list element (if passed a single-element list)
% or the given return value as-is (if passed literally anything else).  Because
% `$RETVAL' is an ordinary OTPCL variable, it's possible to write
% functions/commands that read it, allowing for, say, chained operations on an
% item (this is, notably, how OTPCL's pipe functionality works; more on that in
% the documentation for ``'|'/2'').
return([], State) ->
    {ok, State};
return([RetVal], State) ->
    {RetVal, State};
return(Args, State) ->
    {Args, State}.

-spec '|'([any()|[any()]], state()) -> {any(), state()}.
% @doc Inline command chaining operator.  Takes the result of the preceding
% command (stored in `$RETVAL') and passes it as the first argument to the named
% command (the rest of the arguments passed being passed through as additional
% arguments to the named command).
%
% OTPCL's parser treats any "free" instance of a pipe character (i.e. unescaped
% and not already part of some other word) as a command terminator, so OTPCL
% will interpret a line like `foo | bar | baz' as equivalent to separately
% calling `foo', `| bar', and `| baz' (which would in turn be equivalent to `baz
% [bar [foo]]').  This means that it's possible to define custom commands with
% pipe-like behavior, and rather simply, too; for example, to define a `|!'
% command that sends the result of a command to a process:
%
% ```
% '|!'([Pid], State) ->
%     {RetVal, State} = otpcl_meta:get(['RETVAL', State),
%     Pid ! RetVal,
%     {ok, State}.
% '''
%
% Assuming the above function is tied to a command name somehow, one could then
% call `foo bar baz |! $pid' to send the result of the command `foo bar baz' as
% a message to the process identified via `$pid'.
'|'([Cmd|Args], State) ->
    {RetVal, State} = otpcl_meta:get(['RETVAL'], State),
    otpcl_meta:apply([Cmd, RetVal|Args], State).
