-module(cowboy2_session).

-export([get_session/1, put_session/2, delete_session/1, renew_session_id/1]).

-include_lib("kernel/include/logger.hrl").

-define(COOKIE_NAME, <<"sessionid">>).
-define(TABLE_NAME, cowboy2_session_table).
-define(SESSION_ID_LEN_BYTES, 32).

get_session(_Req = #{session := Session}) ->
    Session.

put_session(Session, Req = #{session_id := SessionId}) ->
    ets:insert(?TABLE_NAME, {SessionId, Session}),
    Req#{?MODULE => {SessionId, Session}}.

delete_session(Req = #{session_id := SessionId}) ->
    ets:delete(?TABLE_NAME, SessionId),
    CookieOpts = #{path => "/", max_age => 0},
    cowboy_req:set_resp_cookie(?COOKIE_NAME, <<>>, Req, CookieOpts).

renew_session_id(_Req = #{has_sent_resp := _}) ->
    % We can't change the session ID in the cookie, because we've already sent the headers to the client.
    error(already_sent);
renew_session_id(Req0 = #{session_id := OldSessionId, session := Session}) ->
    NewSessionId =
        base64url:encode(
            crypto:strong_rand_bytes(?SESSION_ID_LEN_BYTES)),
    ets:delete(?TABLE_NAME, OldSessionId),
    ets:insert(?TABLE_NAME, {NewSessionId, Session}),
    Req = Req0#{session_id => NewSessionId},
    ?LOG_DEBUG(#{msg => renew_session_id,
                 old_session_id => OldSessionId,
                 new_session_id => NewSessionId}),
    % TODO: HttpOnly, Secure, etc.
    CookieOpts = #{path => "/"},
    ?LOG_INFO(#{resp_cookies => maps:get(resp_cookies, Req, #{})}),
    ?LOG_NOTICE(#{set_resp_cookie => NewSessionId}),
    Req2 = cowboy_req:set_resp_cookie(?COOKIE_NAME, NewSessionId, Req, CookieOpts),
    ?LOG_INFO(#{resp_cookies => maps:get(new_resp_cookies, Req2, #{})}),
    Req2.
