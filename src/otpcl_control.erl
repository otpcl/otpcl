% @doc OTPCL flow control commands
%
% Following Tcl's tradition, OTPCL's flow control "structures" are essentially
% just commands that take strings and evaluate them in various ways.  Said
% commands' definitions live in this here module.
-module(otpcl_control).

-include("otpcl.hrl").

-export(['if'/2, unless/2, for/2, while/2, truthy/1, truthy/2]).

-otpcl_cmds(['if', unless, for, while, truthy]).

% @doc Evaluates the second argument if the first is "truthy".  If there's a
% third argument, it'll be evaluated if the first is not "truthy".
'if'([Test, Then], State) ->
    'if'([Test, Then, ""], State);
'if'([Test, Then, Else], State) ->
    case truthy(Test) of
        true ->
            otpcl_eval:eval(Then, State);
        _ ->
            otpcl_eval:eval(Else, State)
    end.

% @doc Inverse of `if'.  Does not accept a third clause, thus disallowing
% "unless/else" constructs (if you want those, go back to Perl -- and I say this
% as someone who loves Perl).
unless([Test, Then], State) ->
    'if'([Test, "", Then], State).

% @doc Repeatedly evals the given clause with values from a list.  Synopsis:
%
% ```
% for each in $list { mangle $each }
% '''
%
% `each' here could be literally any variable name.  Return value will be the
% value returned by the last invocation/evaluation of the body clause.
for([Name, in, [Each|Rest], Do], State) ->
    {ok, NewState} = otpcl_meta:set([Name, Each], State),
    {_, NewerState} = otpcl_eval:eval([Do], NewState),
    for([Name, in, Rest, Do], NewerState);
for([_, in, [], _], State) ->
    {RetVal, State} = otpcl_meta:get(['RETVAL'], State),
    {RetVal, State}.

% @doc Repeatedly evals the second clause for as long as the first clause
% evaluates to a truthy value.  Word of caution: OTPCL currently lacks the
% concept of a `break' statement, so if you ain't careful here you'll end up
% with an infinitely-looping OTPCL interpreter (TODO: fix that).
while([Pred, Do], State) ->
    {PredVal, NewState} = otpcl_eval:eval([Pred], State),
    case truthy(PredVal) of
        true ->
            {_, NewerState} = otpcl_eval:eval([Do], NewState),
            while([Pred, Do], NewerState);
        false ->
            {RetVal, State} = otpcl_meta:get(['RETVAL'], State),
            {RetVal, State}
    end.

% @doc See `truthy/2'.
truthy([]) ->
    false;
truthy(false) ->
    false;
truthy(error) ->
    false;
truthy(0) ->
    false;
truthy(0.0) ->
    false;
truthy(<<>>) ->
    false;
truthy({}) ->
    false;
truthy(T) when is_tuple(T) andalso element(1, T) =:= error ->
    false;
truthy(_) ->
    true.

% @doc Determine if a value is "truthy".  All values are deemed to be "truthy"
% (that is: will cause a control command's predicate to be treated as "true"
% unless they are in any of the following categories:
%
% <ul>
% <li>An empty list, tuple, or binary</li>
% <li>The number zero (either integer `0' or float `0.0')</li>
% <li>The atoms `false' or `error'</li>
% <li>A tuple whose first element is `error'</li>
% </ul>
truthy([Val], State) ->
    {truthy(Val), State}.
