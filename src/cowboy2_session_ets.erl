-module(cowboy2_session_ets).

-export([start_link/0]).

-behaviour(gen_server).

-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-define(TABLE_NAME, cowboy2_session_table).

start_link() ->
    gen_server:start_link(?MODULE, [], []).

init([]) ->
    ?TABLE_NAME = ets:new(?TABLE_NAME, [set, named_table, public]),
    {ok, no_state}.

handle_call(_Req, _From, State) ->
    {reply, ok, State}.

handle_cast(_Req, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
