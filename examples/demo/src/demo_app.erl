-module(demo_app).

-behaviour(application).

-export([start/2, stop/1]).

-define(PORT, 8907).

start(_StartType, _StartArgs) ->
    Dispatch = cowboy_router:compile([{'_', [{"/", home_handler, []}]}]),
    {ok, _} =
        cowboy:start_clear(http,
                           [{port, ?PORT}],
                           #{env => #{dispatch => Dispatch},
                             stream_handlers => [cowboy2_session_stream_h, cowboy_stream_h]}),
    io:format("Listening on http://localhost:~B~n", [?PORT]),
    demo_sup:start_link().

stop(_State) ->
    ok.
