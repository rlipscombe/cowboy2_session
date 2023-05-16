-module(cowboy2_session).

-export([get_session/1, get_session/2, put_session/2, delete_session/1, renew_session_id/1]).

-include_lib("kernel/include/logger.hrl").

-define(COOKIE_NAME, <<"sessionid">>).
-define(TABLE_NAME, cowboy2_session_table).
-define(SESSION_ID_LEN_BYTES, 32).

-spec get_session(Req :: cowboy_req:req()) -> Session :: term().
get_session(Req) ->
    get_session(Req, undefined).

-spec get_session(Req :: cowboy_req:req(), Default :: term()) -> Session :: term().
get_session(_Req = #{session := Session}, _Default) ->
    Session;
get_session(_Req, Default) ->
    Default.

-spec put_session(Session :: term(), Req :: cowboy_req:req()) -> cowboy_req:req().
put_session(Session, Req = #{session_id := SessionId}) ->
    % We have a session ID; update the session.
    ets:insert(?TABLE_NAME, {SessionId, Session}),
    Req#{session => Session};
put_session(Session, Req) ->
    % No session ID; it's a new session.
    SessionId =
        base64url:encode(
            crypto:strong_rand_bytes(?SESSION_ID_LEN_BYTES)
        ),
    ets:insert(?TABLE_NAME, {SessionId, Session}),
    cowboy_req:set_resp_cookie(
        ?COOKIE_NAME,
        SessionId,
        Req#{session_id => SessionId, session => Session},
        get_cookie_opts(Req)
    ).

get_cookie_opts(_Req = #{session_opts := #{cookie_opts := CookieOpts = #{path := _Path}}}) ->
    CookieOpts;
get_cookie_opts(_Req = #{session_opts := #{cookie_opts := CookieOpts}}) ->
    CookieOpts#{path => "/"};
get_cookie_opts(_Req = #{session_opts := _SessionOpts}) ->
    #{path => "/"};
get_cookie_opts(_Req = #{}) ->
    #{path => "/"}.

-spec delete_session(Req :: cowboy_req:req()) -> cowboy_req:req().
delete_session(Req = #{session_id := SessionId}) ->
    ets:delete(?TABLE_NAME, SessionId),
    CookieOpts = #{path => "/", max_age => 0},
    cowboy_req:set_resp_cookie(?COOKIE_NAME, <<>>, maps:remove(session_id, Req), CookieOpts).

-spec renew_session_id(Req :: cowboy_req:req()) -> cowboy_req:req().
renew_session_id(_Req = #{has_sent_resp := _}) ->
    % We can't change the session ID in the cookie, because we've already sent the headers to the client.
    error(already_sent);
renew_session_id(
    Req0 = #{
        session_id := OldSessionId, session := Session, session_opts := #{cookie_opts := CookieOpts}
    }
) ->
    NewSessionId =
        base64url:encode(
            crypto:strong_rand_bytes(?SESSION_ID_LEN_BYTES)
        ),
    ets:delete(?TABLE_NAME, OldSessionId),
    ets:insert(?TABLE_NAME, {NewSessionId, Session}),
    Req = Req0#{session_id => NewSessionId},
    ?LOG_DEBUG(#{
        msg => renew_session_id,
        old_session_id => OldSessionId,
        new_session_id => NewSessionId
    }),
    ?LOG_INFO(#{resp_cookies => maps:get(resp_cookies, Req, #{})}),
    ?LOG_NOTICE(#{set_resp_cookie => NewSessionId}),
    Req2 = cowboy_req:set_resp_cookie(?COOKIE_NAME, NewSessionId, Req, CookieOpts),
    ?LOG_INFO(#{resp_cookies => maps:get(new_resp_cookies, Req2, #{})}),
    Req2.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

get_cookie_opts_enforces_path_test_() ->
    [
        ?_assertMatch(#{path := "/"}, get_cookie_opts(#{})),
        ?_assertMatch(#{path := "/"}, get_cookie_opts(#{session_opts => #{}})),
        ?_assertMatch(#{path := "/"}, get_cookie_opts(#{session_opts => #{cookie_opts => #{}}})),
        ?_assertMatch(
            #{path := "/foo"},
            get_cookie_opts(#{session_opts => #{cookie_opts => #{path => "/foo"}}})
        )
    ].
-endif.
