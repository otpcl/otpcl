-module(otpcl_stdmeta).

-export([import/2, eval/2, 'fun'/2]).

-otpcl_funs([import, eval, 'fun']).

%% import

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
    'fun'([set, Name, fun Module:Name/2], State);
% ...if it's a normal Erlang function (the default assumption)
import([erlang, Module, Name], State) ->
    WrappedFun = fun (Args, S) -> {apply(Module, Name, Args), S} end,
    'fun'([set, Name, WrappedFun], State).

otpcl_funs([{otpcl_funs, Names}|_]) ->
    {otpcl_funs, Names};
otpcl_funs([{_,_}|Rem]) ->
    otpcl_funs(Rem);
otpcl_funs([]) ->
    no_otpcl_funs.


%% eval

eval([erlang, Txt], State) when is_binary(Txt) ->
    eval([erlang, binary_to_list(Txt)], State);
eval([erlang, Txt], State) ->
    {ok, Tokens, _} = erl_scan:string(Txt),
    {ok, Parsed} = erl_parse:parse_exprs(Tokens),
    {value, Result, _} = erl_eval:exprs(Parsed, []),
    {Result, State};
eval([Name, Args], State) ->
    {Fun, State} = 'fun'([get, Name], State),
    apply(Fun, [Args, State]);
eval([Txt], State) ->
    otpcl_eval:eval(Txt, State).


%% This is where the fun begins

'fun'([get, Name], {Funs, Vars}) ->
    case maps:find(Name, Funs) of
        {ok, Fun} ->
            {Fun, {Funs, Vars}};
        _ ->
            {error, {no_such_fun, Name}, {Funs, Vars}}
    end;

'fun'([set, Name, Fun], {Funs, Vars}) when is_function(Fun) ->
    {ok, {maps:put(Name, Fun, Funs), Vars}};

'fun'([call, Fun, Args], State) when is_function(Fun) ->
    apply(Fun, [Args, State]);
'fun'([call, Name, Args], State) ->
    case 'fun'([get, Name], State) of
        Err = {error, _, _} ->
            Err;
        {Fun, State} ->
            apply(Fun, [Args, State])
    end;

'fun'([wrap, Fun], State) when is_function(Fun) ->
    Wrapped = fun (Args, FunState) ->
                      {apply(Fun, Args), FunState}
              end,
    {Wrapped, State}.
