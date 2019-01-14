-module(otpcl_stdlib).

-export([return/2, get/2, set/2, print/2, 'if'/2, truthy/1, unless/2,
         incr/2, decr/2, import/2, eval/2, funget/2, funset/2, funcall/2,
         funwrap/2, send/2]).

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
            {{error, no_such_var}, {Funs, Vars}}
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

% Import the whole module...
import([Module], State) ->
    import([Module, otpcl_funs(Module:module_info(attributes))], State);
% ...if it specifies Made For OTPCL™ functions
import([Module, {otpcl_funs, Names}], State) ->
    import([otpcl, Module, Names], State);
% ...if it doesn't
import([Module, no_otpcl_funs], State) ->
    Names = [Name || {Name, _} <- Module:module_info(exports)],
    import([erlang, Module, Names], State);
% Import a list of functions from a module
import([Type, Module, [Name|Names]], State) ->
    {ok, NewState} = import([Type, Module, Name], State),
    import([Type, Module, Names], NewState);
import([otpcl, Module, []], State) ->
    {{ok, Module}, State};
% Import a specific function...
import([Module, Name], State) ->
    import([erlang, Module, Name], State);
% ...if it's Made For OTPCL™
import([otpcl, Module, Name], State) ->
    funset([Name, fun Module:Name/2], State);
% ...if it's a normal Erlang function (the default assumption)
import([erlang, Module, Name], State) ->
    WrappedFun = fun (Args, S) -> {apply(Module, Name, Args), S} end,
    funset([Name, WrappedFun], State).

otpcl_funs([{otpcl_funs, Names}|_]) ->
    {otpcl_funs, Names};
otpcl_funs([{_,_}|Rem]) ->
    otpcl_funs(Rem);
otpcl_funs([]) ->
    no_otpcl_funs.

eval([erlang, Txt], State) when is_binary(Txt) ->
    eval([erlang, binary_to_list(Txt)], State);
eval([erlang, Txt], State) ->
    {ok, Tokens, _} = erl_scan:string(Txt),
    {ok, Parsed} = erl_parse:parse_exprs(Tokens),
    {value, Result, _} = erl_eval:exprs(Parsed, []),
    {Result, State};
eval([Name, Args], State) ->
    {Fun, State} = funget([Name], State),
    apply(Fun, [Args, State]);
eval([Txt], State) ->
    otpcl_eval:eval(Txt, State).

funget([Name], {Funs, Vars}) ->
    case maps:find(Name, Funs) of
        {ok, Fun} ->
            {Fun, {Funs, Vars}};
        _ ->
            {error, no_such_fun, {Funs, Vars}}
    end.

funset([Name, Fun], {Funs, Vars}) when is_function(Fun) ->
    {ok, {maps:put(Name, Fun, Funs), Vars}}.

funcall([Fun, Args], State) when is_function(Fun) ->
    apply(Fun, [Args, State]);
funcall([Name, Args], State) ->
    {Fun, State} = funget([Name], State),
    apply(Fun, [Args, State]).

funwrap([Fun], State) when is_function(Fun) ->
    Wrapped = fun (Args, FunState) ->
        {apply(Fun, Args), FunState}
    end,
    {Wrapped, State}.

send([Pid, Msg], State) ->
    Pid ! Msg,
    {ok, State}.
