-module(cowboy2_session_middleware).
-export([execute/2]).

-define(TABLE_NAME, cowboy2_session_table).

execute(Req = #{session_id := SessionId, session := Session}, Env) ->
    ets:insert(?TABLE_NAME, {SessionId, Session}),
    {ok, Req, Env}.
