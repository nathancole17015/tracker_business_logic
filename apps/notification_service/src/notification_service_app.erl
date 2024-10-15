-module(notification_service_app).
-behaviour(application).

%% API
-export([start/2, stop/1]).

%% Application callbacks
start(_StartType, _StartArgs) ->
    notification_service_sup:start_link().

stop(_State) ->
    ok.
