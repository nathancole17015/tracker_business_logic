-module(package_registration_server).
-behaviour(gen_server).

%% API
-export([start_link/0, register_package/1, get_package/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(RIAK_BUCKET, <<"test_bucket">>).

-record(state, {client_pid}).

%% Public API
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

register_package(PackageId) ->
    gen_server:call(?MODULE, {register, PackageId}).

get_package(PackageId) ->
    gen_server:call(?MODULE, {get, PackageId}).

%% gen_server Callbacks

init([]) ->
    %% Start connection to Riak
    {ok, ClientPid} = riakc_pb_socket:start_link("database.mertlymedia.net", 8087),
    {ok, #state{client_pid = ClientPid}}.

handle_call({register, PackageId}, _From, #state{client_pid = ClientPid} = State) ->
    Key = integer_to_binary(PackageId),
    Value = jsx:encode(#{locationId => <<"pending">>}),
    Obj = riakc_obj:new(?RIAK_BUCKET, Key, Value),
    case riakc_pb_socket:put(ClientPid, Obj) of
        ok ->
            log_info(io_lib:format("Package ~p registered", [PackageId])),
            {reply, {ok, PackageId}, State};
        {error, Reason} ->
            log_info(io_lib:format("Failed to register package: ~p~n", [Reason])),
            {reply, {error, Reason}, State}
    end;

handle_call({get, PackageId}, _From, #state{client_pid = ClientPid} = State) ->
    Key = integer_to_binary(PackageId),
    case riakc_pb_socket:get(ClientPid, ?RIAK_BUCKET, Key) of
        {ok, Obj} ->
            Value = riakc_obj:get_value(Obj),
            log_info(io_lib:format("Retrieved package ~p", [PackageId])),
            {reply, {ok, Value}, State};
        {error, Reason} ->
            log_info(io_lib:format("Failed to retrieve package: ~p~n", [Reason])),
            {reply, {error, Reason}, State}
    end;

handle_call(_Request, _From, State) ->
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