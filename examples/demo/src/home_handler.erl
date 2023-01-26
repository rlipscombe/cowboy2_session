-module(home_handler).
-export([init/2]).

init(Req0 = #{session := Session}, Opts) ->
    Headers = #{<<"content-type">> => <<"text/plain">>},
    Body = io_lib:format("~9999p", [Session]),
    NewSession = maps:update_with(visits, fun(V) -> V + 1 end, 1, Session),
    Req = cowboy_req:reply(200, Headers, Body, Req0),
    {ok, Req#{session => NewSession}, Opts}.
