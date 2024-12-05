-module(package_registration_handler).
-export([init/2]).

init(Req, _Opts) ->
    {ok, Body, Req1} = cowboy_req:read_body(Req),
    package_registration_sup:start_link(),
    case jsx:decode(Body) of
        [{<<"packageId">>, PackageId}] when is_integer(PackageId) ->
            Result = package_registration_server:register_package(PackageId),
            FormattedResult = io_lib:format("\nRiak Server: ~p",[Result]),
            cowboy_req:reply(200, #{<<"content-type">> => <<"text/plain">>}, FormattedResult, Req1),
            {ok, Req1, #{}};
        [{<<"packageId">>, _PackageId}] ->
            cowboy_req:reply(400, #{<<"content-type">> => <<"text/plain">>}, <<"Invalid Package ID">>, Req1),
            {ok, Req1, #{}};
        Obj ->
            InvalidJsonMsg = io_lib:format("Invalid JSON/ID: ~s", [jsx:encode(Obj)]),
            cowboy_req:reply(400, #{<<"content-type">> => <<"text/plain">>}, InvalidJsonMsg, Req1),
            {ok, Req1, #{}}
    end.