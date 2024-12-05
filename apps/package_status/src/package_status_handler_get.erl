-module(package_status_handler_get).
-export([init/2]).

%% Required for decoding JSON and handling HTTP requests
-include_lib("cowboy/include/cowboy.hrl").

init(Req, _Opts) ->
    {ok, Body, Req1} = cowboy_req:read_body(Req),
    %% Start package_status_server if not already started
    package_status_server:start_link(),
    case jsx:decode(Body, [return_maps]) of
        %% Update package status
        #{<<"action">> := <<"update">>, <<"packageId">> := PackageId, <<"status">> := Status}
            when is_integer(PackageId), is_binary(Status) ->
            Result = package_status_server:update_package_status(PackageId, Status),
            handle_result(Result, Req1);

        %% Get package status
        #{<<"action">> := <<"get">>, <<"packageId">> := PackageId}
            when is_integer(PackageId) ->
            Result = package_status_server:get_package(PackageId),
            handle_result(Result, Req1);

        %% Invalid request
        Obj ->
            InvalidJsonMsg = io_lib:format("Invalid JSON input: ~s", [jsx:encode(Obj)]),
            cowboy_req:reply(400, #{<<"content-type">> => <<"text/plain">>}, InvalidJsonMsg, Req1),
            {ok, Req1, #{}}
    end.

handle_result({ok, Response}, Req) ->
    FormattedResult = io_lib:format("\nResponse: ~p", [Response]),
    cowboy_req:reply(200, #{<<"content-type">> => <<"text/plain">>}, FormattedResult, Req),
    {ok, Req, #{}};

handle_result({error, Reason}, Req) ->
    ErrorMsg = io_lib:format("\nError: ~p", [Reason]),
    cowboy_req:reply(500, #{<<"content-type">> => <<"text/plain">>}, ErrorMsg, Req),
    {ok, Req, #{}}.
