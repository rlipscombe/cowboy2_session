-module(cowboy2_session_stream_h).

-behaviour(cowboy_stream).

-export([init/3, data/4, info/3, terminate/3, early_error/5]).

-include_lib("kernel/include/logger.hrl").

-define(COOKIE_NAME, <<"sessionid">>).
-define(SESSION_ID_LEN_BYTES, 32).
-define(TABLE_NAME, cowboy2_session_table).

init(StreamId, Req0, Opts) ->
    Req = init_session(Req0, Opts),
    {Commands, Next} = cowboy_stream:init(StreamId, Req, Opts),
    {Commands, #{next => Next}}.

init_session(Req, Opts) ->
    Cookies = cowboy_req:parse_cookies(Req),
    init_session_2(lists:keyfind(?COOKIE_NAME, 1, Cookies), Req, Opts).

init_session_2({_, SessionId}, Req, Opts) ->
    ?LOG_DEBUG(#{msg => got_cookie, session_id => SessionId}),
    init_session_3(SessionId, ets:lookup(?TABLE_NAME, SessionId), Req, Opts);
init_session_2(_, Req, Opts) ->
    ?LOG_DEBUG(#{msg => no_cookie}),
    init_new_session(Req, Opts).

init_session_3(SessionId, [{_, Session}], Req, _Opts) ->
    ?LOG_DEBUG(#{msg => existing_session}),
    Req#{session_id => SessionId, session => Session};
init_session_3(_, _, Req, Opts) ->
    init_new_session(Req, Opts).

init_new_session(Req0, Opts) ->
    CookieOpts = get_cookie_opts(Opts),
    NewSessionId =
        base64url:encode(
            crypto:strong_rand_bytes(?SESSION_ID_LEN_BYTES)
        ),
    ?LOG_DEBUG(#{msg => new_session, session_id => NewSessionId}),
    NewSession = #{},
    ets:insert(?TABLE_NAME, {NewSessionId, NewSession}),
    Req = Req0#{session_id => NewSessionId, session => NewSession, session_opts => #{cookie_opts => CookieOpts}},
    ?LOG_NOTICE(#{set_resp_cookie => NewSessionId}),
    cowboy_req:set_resp_cookie(?COOKIE_NAME, NewSessionId, Req, CookieOpts).

get_cookie_opts(_Opts = #{session_opts := #{cookie_opts := CookieOpts}}) ->
    CookieOpts;
get_cookie_opts(_Opts) ->
    #{path => "/"}.

data(StreamId, IsFin, Data, _State = #{next := Next0}) ->
    {Commands, Next} = cowboy_stream:data(StreamId, IsFin, Data, Next0),
    {Commands, #{next => Next}}.

info(StreamId, Info, _State = #{next := Next0}) ->
    {Commands, Next} = cowboy_stream:info(StreamId, Info, Next0),
    {Commands, #{next => Next}}.

terminate(StreamId, Reason, _State = #{next := Next0}) ->
    cowboy_stream:terminate(StreamId, Reason, Next0).

early_error(StreamId, Reason, PartialReq, Resp, Opts) ->
    cowboy_stream:early_error(StreamId, Reason, PartialReq, Resp, Opts).
