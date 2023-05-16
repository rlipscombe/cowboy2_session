-module(cowboy2_session_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link(?MODULE, []).

%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional
init([]) ->
    SupFlags =
        #{
            strategy => one_for_one,
            intensity => 0,
            period => 1
        },
    ChildSpecs = [#{id => cowboy2_session_ets, start => {cowboy2_session_ets, start_link, []}}],
    {ok, {SupFlags, ChildSpecs}}.
