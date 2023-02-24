-module(cowboy2_session_test_handler).
-export([init/2]).

init(Req, _Opts = [Fun]) when is_map(Req), is_function(Fun, 2) ->
    Fun(Req, []).
