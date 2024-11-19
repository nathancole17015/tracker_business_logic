-module(package_registration_handler).
-export([init/2]).

init(Req, _Opts) ->
    %% Read the request body
    {ok, Body, Req1} = cowboy_req:read_body(Req),

    %% Decode the JSON body to extract the package ID
    case jsx:decode(Body) of
        {[{<<"packageId">>, PackageIdStr}]} ->
            %% Convert the PackageId to an integer
            case string:to_integer(PackageIdStr) of
                {error, _} ->
                    %% Respond with a 400 Bad Request if the PackageId is invalid
                    cowboy_req:reply(400, #{<<"content-type">> => <<"text/plain">>}, <<"Invalid Package ID">>, Req1),
                    {ok, Req1, #{}};
                {PackageId,_} ->
                    %% Call package_registration_server:register_package/1
                    Result = package_registration_server:register_package(PackageId),

                    %% Format and send the response
                    cowboy_req:reply(200, #{<<"content-type">> => <<"application/json">>}, jsx:encode(Result), Req1),
                    {ok, Req1, #{}}
            end;
        _ ->
            %% Respond with 400 Bad Request if the body is invalid
            cowboy_req:reply(400, #{<<"content-type">> => <<"text/plain">>}, <<"Invalid JSON">>, Req1),
            {ok, Req1, #{}}
    end.
