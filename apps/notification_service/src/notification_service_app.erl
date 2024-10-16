%%%-------------------------------------------------------------------
%% @doc notification_service public API
%% @end
%%%-------------------------------------------------------------------

-module(notification_service_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    notification_service_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
