%%%-------------------------------------------------------------------
%% @doc package_registration public API
%% @end
%%%-------------------------------------------------------------------

-module(package_registration_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    package_registration_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
