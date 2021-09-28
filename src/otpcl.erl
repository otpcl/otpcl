% @doc One-stop shop for all your OTPCL needs.
%
% The `otpcl' module re-exports various functions for convenience when using
% OTPCL from within other OTP applications.
-module(otpcl).

-behaviour(application).

-include("otpcl.hrl").

-export([start/2, stop/1, scan/1, scan/2, parse/1, interpret/1, interpret/2,
         eval/1, eval/2, eval_file/1, eval_file/2, get/2, set/3, import/2,
         import/3, cmd/2, cmd/3]).

% @hidden
start(_Type, _Args) ->
    {error, not_implemented}.

% @hidden
stop(_State) ->
    ok.

% Re-exposing some of the module functions for convenience ('cause
% it's annoying to have to type otpcl_foo:foo() all the time).

-spec scan(str_or_bin()) -> [token()].
% @doc Convert a charlist string or binary string into a token string.  The
% resulting tokens will start from the default starting position
% (`{nofile,0,0}').
scan(Txt) ->
    otpcl_parse:scan(Txt).

-spec scan(str_or_bin(), position()) -> [token()].
% @doc Convert a charlist string or binary string into a token string.  The
% resulting tokens will start from the starting position `Pos'.
scan(Txt, Pos) ->
    otpcl_parse:scan(Txt, Pos).

-spec parse(eval_input()) -> parse_success() | parse_error().
% @doc Parse a (charlist/binary/token) string as an OTPCL program.
parse(Src) ->
    otpcl_parse:parse(Src).

-spec interpret(tree()) -> eval_success() | eval_error().
% @doc Interpret an OTPCL parse tree with the default starting state.
interpret(Tree) ->
    otpcl_eval:interpret(Tree).

-spec interpret(tree(), state()) -> eval_success() | eval_error().
% @doc Interpret an OTPCL parse tree with a custom starting state.
interpret(Tree, State) ->
    otpcl_eval:interpret(Tree, State).

-spec eval(eval_input()) -> eval_success() | eval_error().
% @doc Interpret a (charlist/binary/token) string as an OTPCL program with the
% default starting state.
eval(Src) ->
    otpcl_eval:eval(Src).

-spec eval(eval_input(), state()) -> eval_success() | eval_error().
% @doc Interpret a (charlist/binary/token) string as an OTPCL program with a
% custom starting state.
eval(Src, State) ->
    otpcl_eval:eval(Src, State).

-spec eval_file(filename()) -> eval_success() | eval_error().
% @doc Interpret the contents of a file as an OTPCL program with the default
% starting state.
eval_file(Filename) ->
    otpcl_eval:eval_file(Filename).

-spec eval_file(filename(), state()) -> eval_success() | eval_error().
% @doc Interpret the contents of a file as an OTPCL program with a custom
% starting state.
eval_file(Filename, State) ->
    otpcl_eval:eval_file(Filename, State).

-spec get(atom(), state()) -> {any(), state()}.
% @doc Get the value of a variable from an OTPCL interpreter state.
get(Name, State) ->
    otpcl_meta:get(Name, State).

-spec set(atom(), any(), state()) -> {'ok', state()}.
% @doc Set the value of a variable in an OTPCL interpreter state.
set(Name, Val, State) ->
    otpcl_meta:set(Name, Val, State).

-spec import(atom(), state()) -> {'ok',state()} | {{'ok',atom()},state()}.
% @doc Import all the functions from a module into an OTPCL interpreter state.
import(Module, State) ->
    otpcl_meta:import(Module, State).

-spec import(atom(), atom(), state()) -> {'ok',state()} | {{'ok',atom()},state()}.
% @doc Import a specific function from a module into an OTPCL interpreter state.
import(Module, Fun, State) ->
    otpcl_meta:import(Module, [Fun], State).

-spec cmd(atom(), state()) -> {function(), state()}.
% @doc Get the function backing a command from an OTPCL interpreter state.
cmd(Name, State) ->
    otpcl_meta:cmd(Name, State).

-spec cmd(atom(), function(), state()) -> {'ok', state()}.
% @doc Set the function backing a command in an OTPCL interpreter state.
cmd(Name, Fun, State) ->
    otpcl_meta:cmd(Name, Fun, State).
