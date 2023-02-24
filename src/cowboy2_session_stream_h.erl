-module(cowboy2_session_stream_h).

%-behaviour(cowboy_stream).

-export([init/3, data/4, info/3, terminate/3, early_error/5]).

-include_lib("kernel/include/logger.hrl").

-define(COOKIE_NAME, <<"sessionid">>).
-define(TABLE_NAME, cowboy2_session_table).

init(StreamId, Req0, Opts) ->
    SessionOpts = maps:get(session_opts, Opts, #{}),
    Req = init_session(Req0, SessionOpts),
    {Commands, Next} = cowboy_stream:init(StreamId, Req, Opts),
    {Commands, #{next => Next}}.

init_session(Req, SessionOpts) ->
    Cookies = cowboy_req:parse_cookies(Req),
    init_session_2(lists:keyfind(?COOKIE_NAME, 1, Cookies), Req, SessionOpts).

init_session_2({_, SessionId}, Req, SessionOpts) ->
    % We've got a session cookie; is there a session object in ETS?
    ?LOG_DEBUG(#{msg => got_cookie, session_id => SessionId}),
    init_session_3(SessionId, ets:lookup(?TABLE_NAME, SessionId), Req, SessionOpts);
init_session_2(_, Req, SessionOpts) ->
    % We didn't get a session cookie.
    ?LOG_DEBUG(#{msg => no_cookie}),
    Req#{session_opts => SessionOpts}.

init_session_3(SessionId, [{_, Session}], Req, SessionOpts) ->
    % We found the session in ETS.
    ?LOG_DEBUG(#{msg => existing_session}),
    Req#{session_id => SessionId, session => Session, session_opts => SessionOpts};
init_session_3(_, _, Req, SessionOpts) ->
    % We didn't find the session in ETS.
    Req#{session_opts => SessionOpts}.

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
