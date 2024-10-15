-module(notification_service_sup).
-behaviour(supervisor).

%% API
-export([start_link/0]).
-export([init/1]).

%% Supervisor callback
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    SupFlags = #{strategy => one_for_one, intensity => 1, period => 5},
    ChildSpecs = [
        #{id => notification_service_event_manager,
          start => {notification_service_event_manager, start_link, []},
          restart => permanent,
          shutdown => 5000,
          type => worker
        }
    ],
    {ok, {SupFlags, ChildSpecs}}.
