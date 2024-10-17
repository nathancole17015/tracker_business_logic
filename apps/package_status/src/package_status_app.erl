%%%-------------------------------------------------------------------
%% @doc package_status public API
%% @end
%%%-------------------------------------------------------------------

-module(package_status_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    package_status_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
