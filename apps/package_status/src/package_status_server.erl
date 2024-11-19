-module(package_status_server).
-behaviour(gen_server).

%% API
-export([start_link/0, update_package_status/2, get_package/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(RIAK_URL, "http://database.mertlymedia.net:8098/buckets/test_bucket/keys/").

-record(state, {}).

%% Public API
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

update_package_status(PackageId, Status) ->
    gen_server:call(?MODULE, {update_status, PackageId, Status}).

get_package(PackageId) ->
    gen_server:call(?MODULE, {get, PackageId}).

%% gen_server Callbacks

init([]) ->
    %% Start inets (for httpc)
    inets:start(),
    {ok, #state{}}.

handle_call({update_status, PackageId, Status}, _From, State) ->
    %% Define the URL for the specific package
    URL = ?RIAK_URL ++ integer_to_list(PackageId),
    Data = jsx:encode([{status, Status}]),
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
    %% Send HTTPS PUT request to update the package status
    case httpc:request(put, {URL, Headers, "application/json", Data}, [], SSL_Options) of
        {ok, {{_, 200, _}, _, _}} ->
            log_info(io_lib:format("Package ~p status updated to ~p", [PackageId, Status])),
            {reply, {ok, PackageId}, State};
        {ok, {{_, StatusCode, _}, _, ResponseBody}} ->
            log_info(io_lib:format("Failed to update package, status: ~p, response: ~s~n", [StatusCode, ResponseBody])),
            {reply, {error, {http_error, StatusCode}}, State};
        {error, Reason} ->
            log_info(io_lib:format("Failed to update package: ~p~n", [Reason])),
            {reply, {error, Reason}, State}
    end;

handle_call({get, PackageId}, _Sender, State) ->
    %% Define the URL for the specific package
    URL = ?RIAK_URL ++ integer_to_list(PackageId),
    %% Send HTTP GET request to retrieve data
    case httpc:request(get, {URL, []}, [], []) of
        {ok, {{_, 200, _}, _, Body}} ->
            log_info(io_lib:format("Package ~p retrieved", [PackageId])),
            {reply, {ok, jsx:decode(list_to_binary(Body), [return_maps])}, State};
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
    io:format("~s~n", [Msg]).
-endif.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

log_info(_Msg) ->
    ok.

%% Tests
setup() ->
    meck:new(httpc, [passthrough]),
    {ok, package_registration_server:start_link()}.

teardown(_) ->
    meck:unload(httpc).

update_package_status_test() ->
    %% Mock a successful HTTP PUT request
    meck:expect(httpc, request, fun(_Method, {_URL, _Headers, _Type, _Data}, _Options, _Opts) ->
        {ok, {{http, 200, "OK"}, [], "Status updated"}}
    end),

    %% Test updating the package status
    PackageId = 123,
    Status = <<"shipped">>,
    Result = package_registration_server:update_package_status(PackageId, Status),
    ?assertMatch({ok, PackageId}, Result).

get_package_test() ->
    %% Mock a successful HTTP GET request
    meck:expect(httpc, request, fun(get, {_URL, _Headers}, _Options, _Opts) ->
        {ok, {{http, 200, "OK"}, [], "{\"status\":\"shipped\"}"}}
    end),

    %% Test getting the package
    PackageId = 123,
    Result = package_registration_server:get_package(PackageId),
    ExpectedData = #{<<"status">> => <<"shipped">>},
    ?assertMatch({ok, ExpectedData}, Result).

-endif.