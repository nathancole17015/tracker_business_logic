%%%-------------------------------------------------------------------
%% @doc package_registration public API
%% @end
%%%-------------------------------------------------------------------

-module(package_registration_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    %% Specify the SSL options with the paths to your certificates
    SSL_Opts = [
        {certfile, "./priv/ssl/fullchain.pem"},
        {keyfile, "./priv/ssl/privkey.pem"}
    ],

    %% Set up Cowboy with HTTPS on port 8443
    {ok, _} = cowboy:start_https(
        my_https_listener,
        100,  %% Max connections
        #{port => 8443},
        #{env => #{dispatch => cowboy_router:compile([
            {'_', [
                {"/register", package_registration_handler, []}
            ]}
        ])}},
        SSL_Opts
    ),
    package_registration_server:start_link().

stop(_State) ->
    ok.

%% internal functions
