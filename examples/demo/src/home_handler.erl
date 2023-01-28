-module(home_handler).
-export([init/2]).

init(Req, Opts) ->
    Session = cowboy2_session:get_session(Req),

    Headers = #{<<"content-type">> => <<"text/plain">>},
    Body = io_lib:format("~9999p", [Session]),

    NewSession = maps:update_with(visits, fun(V) -> V + 1 end, 1, Session),

    % Note that you can't renew the session ID after calling cowboy_req:reply,
    % because the cookie has already been sent to the client.
    Req2 = cowboy2_session:renew_session_id(Req),
    Req3 = cowboy2_session:put_session(NewSession, Req2),

    Req4 = cowboy_req:reply(200, Headers, Body, Req3),
    {ok, Req4, Opts}.
