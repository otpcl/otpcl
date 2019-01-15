-module(otpcl_stdlib).

-export([return/2, get/2, set/2, print/2, 'if'/2, truthy/1, unless/2,
         incr/2, decr/2, send/2]).

% Modules which provide Made For OTPCL™ functions can specify them
% with the -otpcl_funs/1 module attribute, which will tell OTPCL's
% import function(s) to import these as state-changing functions
% (otherwise, it'll import them as if they're ordinary Erlang
% functions, which might not be what you want if you actually do care
% about the interpreter state).
-otpcl_funs([return, get, set, print, 'if', unless, incr, decr, import, eval,
             funget, funset, funcall, funwrap, send]).

% All OTPCL functions are represented behind-the-scenes as 2-arity
% Erlang functions; the first argument is a list of the actual
% arguments, and the second argument is the interpreter state.  Every
% OTPCL function's backing Erlang function in turn returns a tuple
% with a return value and an updated interpreter state.

% This standard library is pretty bare-bones, but should provide an
% adequate demonstration of how to define further functions.

return([], State) ->
    {ok, State};
return([RetVal], State) ->
    {RetVal, State};
return(Args, State) ->
    {Args, State}.

get([Name], {Funs, Vars}) ->
    case maps:find(Name, Vars) of
        {ok, Val} ->
            {Val, {Funs, Vars}};
        _ ->
            {error, {no_such_var, Name}, {Funs, Vars}}
    end.

set([Name, Val], {Funs, Vars}) ->
    {ok, {Funs, maps:put(Name, Val, Vars)}}.

print([], State) ->
    {RetVal, State} = get(['RETVAL'], State),
    {io:format("~p", [RetVal]), State};
print([Out], State) ->
    case 'string?'([Out], State) of
        {true, State} ->
            {io:format(Out), State};
        _ ->
            {io:format("~p", [Out]), State}
    end;
print([Text|Args], State) ->
    {io:format(Text, Args), State}.
    
'string?'([Text], State) ->
    {is_binary(Text) or io_lib:char_list(Text), State}.
    
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
    {OldVal, State} = get([VarName], State),
    NewVal = OldVal + By,
    {ok, NewState} = set([VarName, NewVal], State),
    {NewVal, NewState}.

decr([], State) ->
    incr([-1], State);
decr([VarName], State) when is_atom(VarName) ->
    incr([VarName, -1], State);
decr([By], State) ->
    incr([By * -1], State);
decr([VarName, By], State) ->
    incr([VarName, By * -1], State).

send([Pid, Msg], State) ->
    Pid ! Msg,
    {ok, State}.
