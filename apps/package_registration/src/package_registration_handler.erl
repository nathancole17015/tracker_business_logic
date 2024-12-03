-module(package_registration_handler).
-export([init/2]).

init(Req, _Opts) ->
    {ok, Body, Req1} = cowboy_req:read_body(Req),
    package_registration_sup:start_link(),
    case jsx:decode(Body) of
        [{<<"packageId">>, PackageIdStr}] ->
            case string:to_integer(PackageIdStr) of
                {error, _} ->
                    cowboy_req:reply(400, #{<<"content-type">> => <<"text/plain">>}, <<"Invalid Package ID">>, Req1),
                    {ok, Req1, #{}};
                {PackageId,_} ->
                    Result = package_registration_server:register_package(PackageId),
                    FormattedResult = io_lib:format("~n**********~nRiak Server: ~p~n**********~n",[Result]),
                    cowboy_req:reply(200, #{<<"content-type">> => <<"text/plain">>}, FormattedResult, Req1),
                    {ok, Req1, #{}}
            end;
        Obj ->
            InvalidJsonMsg = io_lib:format("Invalid JSON: ~s", [jsx:encode(Obj)]),
            cowboy_req:reply(400, #{<<"content-type">> => <<"text/plain">>}, InvalidJsonMsg, Req1),
            {ok, Req1, #{}}
    end.
