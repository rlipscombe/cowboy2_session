-module(cowboy2_session_test).
-include_lib("eunit/include/eunit.hrl").

-define(SESSION_ID, <<"ab_cde-fghijk_lmno-pqrst-uvwxyz0_12345-6789">>).
-define(DEFAULT_SESSION, #{}).
-define(SESSION_TABLE, cowboy2_session_table).

session_test_() ->
    {setup, fun suite_setup/0, fun suite_cleanup/1,
        {foreach, fun setup/0, fun cleanup/1, [
            fun get_session_after_put_session/0,
            fun without_put_session__no_cookie_is_returned/0,
            fun with_put_session__session_is_saved_and_cookie_is_returned/0,
            fun get_session__returns_default/0,
            fun with_cookie_and_session__get_previous_session/0
        ]}}.

suite_setup() ->
    {ok, _} = application:ensure_all_started(cowboy),
    {ok, _} = application:ensure_all_started(cowboy2_session),
    ok.

suite_cleanup(_) ->
    ok.

setup() ->
    ets:delete_all_objects(?SESSION_TABLE),
    ok.

cleanup(_) ->
    ok.

get_session_after_put_session() ->
    Session = #{is_admin => true, wears_a_hat => true},
    Req0 = #{session_id => ?SESSION_ID, session => ?DEFAULT_SESSION},
    Req1 = cowboy2_session:put_session(Session, Req0),
    ?assertEqual(Session, cowboy2_session:get_session(Req1)).

with_cowboy(HandlerFun) ->
    with_cowboy([], HandlerFun).

with_cowboy(Headers, HandlerFun) ->
    Listener = ?MODULE,
    Dispatch = cowboy_router:compile([
        {"localhost", [{"/", cowboy2_session_test_handler, [HandlerFun]}]}
    ]),
    {ok, _} = cowboy:start_clear(Listener, [{port, 0}], #{
        env => #{dispatch => Dispatch},
        stream_handlers => [cowboy2_session_stream_h, cowboy_stream_h]
    }),
    Port = ranch:get_port(Listener),
    try
        {ok, {{_, StatusCode, _}, RespHeaders, RespBody}} = httpc:request(
            get, {io_lib:format("http://localhost:~B/", [Port]), Headers}, [], []
        ),
        {ok, StatusCode, RespHeaders, RespBody}
    after
        cowboy:stop_listener(Listener)
    end.

without_put_session__no_cookie_is_returned() ->
    {ok, 200, Headers, _} =
        with_cowboy(fun(Req, Opts) ->
            % don't call put_session; there should be no cookie returned.
            {ok, cowboy_req:reply(200, #{}, <<"OK">>, Req), Opts}
        end),
    ?assert(not has_set_cookie(Headers)),
    ?assertEqual([], ets:tab2list(?SESSION_TABLE)),
    ok.

with_put_session__session_is_saved_and_cookie_is_returned() ->
    {ok, 200, Headers, _} =
        with_cowboy(fun(Req, Opts) ->
            Session = #{is_admin => true, wears_a_hat => true},
            Req2 = cowboy2_session:put_session(Session, Req),
            {ok, cowboy_req:reply(200, #{}, <<"OK">>, Req2), Opts}
        end),
    % I considered getting the session ID from the cookie, but that assumes it's used directly as the table key.
    % If we ever implement session ID hashing (#10), that'll break.
    ?assert(has_set_cookie(Headers)),
    ?assertMatch([{_SessionId, #{}}], ets:tab2list(?SESSION_TABLE)),
    ok.

get_session__returns_default() ->
    {ok, 200, _, _} = with_cowboy(fun(Req, Opts) ->
        ?assertEqual(undefined, cowboy2_session:get_session(Req)),
        {ok, cowboy_req:reply(200, #{}, <<"OK">>, Req), Opts}
    end),
    ok.

with_cookie_and_session__get_previous_session() ->
    Session = #{is_admin => true, wears_a_hat => true},
    {ok, 200, RespHeaders, _} =
        with_cowboy(fun(Req, Opts) ->
            Req2 = cowboy2_session:put_session(Session, Req),
            {ok, cowboy_req:reply(200, #{}, <<"OK">>, Req2), Opts}
        end),
    {_, SetCookie} = lists:keyfind("set-cookie", 1, RespHeaders),
    {ok, CookieName, CookieValue, _CookieOpts} = cow_cookie:parse_set_cookie(
        list_to_binary(SetCookie)
    ),
    ReqHeaders = [{"cookie", io_lib:format("~s=~s", [CookieName, CookieValue])}],
    {ok, 200, _, _} = with_cowboy(ReqHeaders, fun(Req, Opts) ->
        ?assertEqual(Session, cowboy2_session:get_session(Req)),
        {ok, cowboy_req:reply(200, #{}, <<"OK">>, Req), Opts}
    end),
    ok.

has_set_cookie(Headers) ->
    lists:any(
        fun
            ({"set-cookie", _}) -> true;
            (_) -> false
        end,
        Headers
    ).

% TODO: More tests:
% - renew_session deletes the existing session, creates a new one, and returns a different cookie.
% - the client can't control the session ID in the cookie. If they give us a cookie that doesn't exist in ETS, we'll ignore it.
