-module(otpcl).

-behaviour(application).

-export([start/2, stop/1, parse/1, interpret/1, interpret/2, eval/1,
         eval/2]).

start(_Type, _Args) ->
    {error, not_implemented}.

stop(_State) ->
    ok.

% Re-exposing some of the module functions for convenience ('cause
% it's annoying to have to type otpcl_foo:foo() all the time).

parse(Src) ->
    otpcl_parse:parse(Src).

interpret(Tree) ->
    otpcl_eval:interpret(Tree).
interpret(Tree, State) ->
    otpcl_eval:interpret(Tree, State).

eval(Src) ->
    otpcl_eval:eval(Src).
eval(Src, State) ->
    otpcl_eval:eval(Src, State).
