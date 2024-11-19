-module(package_registration_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    % {ok, _StartedApps} = application:ensure_all_started(cowboy),

    Dispatch = cowboy_router:compile([
            {'_', [
                {"/register", package_registration_handler, []}
                %{"/", cowboy_static, {priv_file, db_access, "static/index.html"}}        
            ]}
        ]),
    ParentPrivDir = filename:dirname(filename:dirname(code:priv_dir(package_registration))),
    %tls stands for transport layer security
    {ok,_} = cowboy:start_tls(https_listener, [
            {port, 8443},
            {certfile, ParentPrivDir ++ "/priv/ssl/fullchain.pem"},
            {keyfile, ParentPrivDir ++ "/priv/ssl/privkey.pem"}],
            #{env => #{dispatch => Dispatch}}
        ),

    %% Start the TLS listener
    package_registration_sup:start_link().



stop(_State) ->
    ok.

