-type str_or_bin() :: string() | binary().

-type filename() :: any().
-type line_no() :: integer().
-type column_no() :: integer().
-type position() :: {filename(), line_no(), column_no()}.

-type token() :: {char(), position()}.

-type level() :: atom().
-type tree() :: {'parsed', level(), [tree()] | [token()]}.

-type parse_success() :: {'ok', tree(), [token()]}.
-type parse_error() :: {'error', reason(), level(), [token()], [tree()]}.
-type reason() :: atom() | {atom(), any()}.

-type funs() :: map().
-type vars() :: map().
-type state() :: {funs(), vars()}.

-type eval_input() :: string() | binary() | [token()].
-type eval_success() :: {'ok', state()}.
-type eval_error() :: {'error', reason(), state()}.
