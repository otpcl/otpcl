-module(otpcl_stdlib).

-export([set/2, print/2, 'if'/2, truthy/1, unless/2, incr/2, decr/2, import/2]).

% All OTPCL functions are represented behind-the-scenes as 2-arity
% Erlang functions; the first argument is a list of the actual
% arguments, and the second argument is the interpreter state.  Every
% OTPCL function's backing Erlang function in turn returns a tuple
% with a return value and an updated interpreter state.

% This standard library is pretty bare-bones, but should provide an
% adequate demonstration of how to define further functions.

set([Name, Val], State) ->
    {ok, Name, Val, NewState} = otpcl_env:set_var(Name, Val, State),
    {ok, NewState}.

print([], State) ->
    {ok, 'RETVAL', RetVal, State} = otpcl_env:get_var('RETVAL', State),
    {io:format("~p", [RetVal]), State};
print([Text], State) ->
    {io:format(Text), State};
print([Text, Args], State) ->
    {io:format(Text, Args), State}.

'if'([Test, Then], State) ->
    'if'([Test, Then, ""], State);
'if'([Test, Then, Else], State) ->
    case truthy(otpcl_eval:eval(Test, State)) of
        true ->
            otpcl_eval:eval(Then, State);
        _ ->
            otpcl_eval:eval(Else, State)
    end.

% Just a helper for truthiness
truthy([]) ->
    false;
truthy(false) ->
    false;
truthy(error) ->
    false;
truthy(0) ->
    false;
truthy(<<>>) ->
    false;
truthy({}) ->
    false;
truthy(T) when is_tuple(T) andalso element(1, T) =:= error ->
    false;
truthy(_) ->
    true.

unless([Test, Then], State) ->
    'if'([Test, "", Then], State).

incr([], State) ->
    incr([1], State);
incr([VarName], State) when is_atom(VarName) ->
    incr([VarName, 1], State);
incr([By], State) ->
    incr(['RETVAL', By], State);
incr([VarName, By], State) ->
    {ok, VarName, OldVal, State} = otpcl_env:get_var(VarName, State),
    NewVal = OldVal + By,
    {ok, VarName, NewVal, NewState} = otpcl_env:set_var(VarName, NewVal,
                                                        State),
    {NewVal, NewState}.

decr([], State) ->
    incr([-1], State);
decr([VarName], State) when is_atom(VarName) ->
    incr([VarName, -1], State);
decr([By], State) ->
    incr([By * -1], State);
decr([VarName, By], State) ->
    incr([VarName, By * -1], State).

import([Module, Name], State) ->
    {ok, Name, Fun, NewState} = otpcl_env:import_fun(Module, Name, State),
    {Fun, NewState}.
