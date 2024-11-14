-module(package_registration_server).
-behaviour(gen_server).

%% API
-export([start_link/0, register_package/1, get_package/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(RIAK_URL, "http://database.mertlymedia.net:8098/buckets/test_bucket/keys/").

-record(state, {}).

%% Public API
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

register_package(PackageId) ->
    gen_server:call(?MODULE, {register, PackageId}).

get_package(PackageId) ->
    gen_server:call(?MODULE, {get, PackageId}).

%% gen_server Callbacks

init([]) ->
    %% Start inets (for httpc)
    inets:start(),
    {ok, #state{}}.

handle_call({register, PackageId}, _From, State) ->
    %% Define the URL for the specific package
    URL = ?RIAK_URL
        ++ integer_to_list(PackageId)
        ++ "?returnbody=true",
    Data = jsx:encode([{locationId, <<"pending">>}]),
    %% Headers for JSON content
    Headers = [{"Content-Type", "application/json"}],
    %% SSL Options for httpc
    SSL_Options = [
        {ssl, [
            {verify, verify_peer},
            {cacertfile, "./priv/ssl/fullchain.pem"},
            {keyfile, "./priv/ssl/privkey.pem"},
            {depth, 2}
        ]}
    ],
    %% Send HTTPS PUT request to store data
    case httpc:request(put, {URL, Headers, "application/json", Data}, [], SSL_Options) of
        {ok, {{_, 200, _}, _, _}} ->
            log_info(io_lib:format("Package ~p registered", [PackageId])),
            {reply, {ok, PackageId}, State};
        {ok, {{_, StatusCode, _}, _, ResponseBody}} ->
            log_info(io_lib:format("Failed to register package, status: ~p, response: ~s~n", [StatusCode, ResponseBody])),
            {reply, {error, {http_error, StatusCode}}, State};
        {error, Reason} ->
            log_info(io_lib:format("Failed to register package: ~p~n", [Reason])),
            {reply, {error, Reason}, State}
    end;

handle_call({get, PackageId}, _Sender, State) ->
    %% Define the URL for the specific package
    URL = ?RIAK_URL ++ integer_to_list(PackageId),
    %% Send HTTP GET request to retrieve data
    case httpc:request(get, {URL, []}, [], []) of
        {ok, {{_, 200, _}, _, Body}} ->
            log_info(io_lib:format("Package ~p registered", [PackageId])),
            {reply, {ok, jsx:decode(list_to_binary(Body),[return_maps])}, State};
        {ok, {{_, StatusCode, _}, _, _}} ->
            log_info(io_lib:format("Failed to retrieve package, status: ~p~n", [StatusCode])),
            {reply, {error, {http_error, StatusCode}}, State};
        {error, Reason} ->
            log_info(io_lib:format("Failed to retrieve package: ~p~n", [Reason])),
            {reply, {error, Reason}, State}
    end;

handle_call(_Request, _Sender, State) ->
    {reply, error, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


-ifndef(TEST).
log_info(Msg) ->
    io:format("~s~n",[Msg]).
-endif.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

log_info(_Msg) ->
    ok.
%% Setup and Teardown for tests
setup() ->
    meck:new(httpc, [passthrough]),
    {ok, package_registration_server:start_link()}.

teardown(_) ->
    meck:unload(httpc).

%% Tests
start_link_test() ->
    {ok, Pid} = package_registration_server:start_link(),
    is_process_alive(Pid).

register_package_success_test() ->
    %% Mock a successful HTTP PUT request
    meck:expect(httpc, request, fun(_Method, {_URL, _Headers, _Type, _Data}, _Options, _Opts) ->
        {ok, {{http, 200, "OK"}, [], "Package registered"}}
    end),

    %% Call register_package/3 and check for successful response
    PackageId = 123,
    Result = package_registration_server:register_package(PackageId),
    ?assertMatch({ok, PackageId}, Result).

register_package_failure_test() ->
    %% Mock an HTTP error response for PUT
    meck:expect(httpc, request, fun(_Method, {_URL, _Headers, _Type, _Data}, _Options, _Opts) ->
        {ok, {{http, 500, "Internal Server Error"}, [], "Failure"}}
    end),

    %% Attempt to register a package, expecting an error tuple in response
    Result = package_registration_server:register_package(456),
    ?assertMatch({error, {http_error, 500}}, Result).

get_package_success_test() ->
    %% Mock a successful HTTP GET request
    meck:expect(httpc, request, fun(get, {_URL, _Headers}, _Options, _Opts) ->
        {ok, {{http, 200, "OK"}, [], "{\"locationId\":\"pending\"}"}}
    end),

    %% Test get_package/1 with an expected JSON response
    PackageId = 123,
    Result = package_registration_server:get_package(PackageId),
    
    %% Adjust ExpectedData to use map syntax with binary keys and values
    ExpectedData = #{<<"locationId">> => <<"pending">>},
    ?assertMatch({ok, ExpectedData}, Result).


get_package_failure_test() ->
    %% Mock a failure for HTTP GET
    meck:expect(httpc, request, fun(get, {_URL, _Headers}, _Options, _Opts) ->
        {ok, {{http, 404, "Not Found"}, [], "Not found"}}
    end),

    %% Attempt to get a non-existing package, expecting an error
    Result = package_registration_server:get_package(789),
    ?assertMatch({error, {http_error, 404}}, Result).

-endif.