-module(package_registration_handler).
-export([init/2, handle_request/2]).

init(Req, _Opts) ->
    %% Handle the request here, e.g., by calling package_registration_server functions
    {ok, Req, #{}}.
