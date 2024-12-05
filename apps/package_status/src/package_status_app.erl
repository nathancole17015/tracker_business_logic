-module(package_status_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    {ok, _StartedApps} = application:ensure_all_started(cowboy),

    Dispatch = cowboy_router:compile([
            {'_', [
                {"/status", package_status_handler_get, []}
            ]}
        ]),
    ParentPrivDir = lists:foldr(
        fun (_,Dir) -> filename:dirname(Dir) end,
        code:priv_dir(package_status),
        lists:seq(1,5)
    ),

    {ok,_} = cowboy:start_tls(https_listener, [
            {port, 8443},
            {certfile, ParentPrivDir ++ "/priv/ssl/fullchain.pem"},
            {keyfile, ParentPrivDir ++ "/priv/ssl/privkey.pem"}],
            #{env => #{dispatch => Dispatch}}
        ),

    %% Start the TLS listener
    package_status_sup:start_link().

stop(_State) ->
    ok.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
prin_test() ->
    ?assertMatch(fail,fail).
-endif.