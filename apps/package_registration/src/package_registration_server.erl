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
-ifndef(TEST).
init([]) ->
    %% Start connection to Riak
    {ok, ClientPid} = riakc_pb_socket:start_link("database.mertlymedia.net", 8087),
    {ok, #state{client_pid = ClientPid}}.
-endif.
handle_call({register, PackageId}, _From, #state{client_pid = ClientPid} = State) when is_integer(PackageId) ->
    Key = integer_to_binary(PackageId),
    Value = jsx:encode(#{locationId => <<"pending">>}),
    Obj = riakc_obj:new(?RIAK_BUCKET, Key, Value),
    case riakc_pb_socket:put(ClientPid, Obj) of
        ok ->
            {reply, {ok, PackageId}, State};
        {error, Reason} ->
            log_info(io_lib:format("Failed to register package: ~p~n", [Reason])),
            {reply, {error, Reason}, State}
    end;

handle_call({get, PackageId}, _From, #state{client_pid = ClientPid} = State) when is_integer(PackageId) ->
    Key = integer_to_binary(PackageId),
    case riakc_pb_socket:get(ClientPid, ?RIAK_BUCKET, Key) of
        {ok, Obj} ->
            Value = riakc_obj:get_value(Obj),
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

init([]) -> {ok, #state{client_pid = self()}}.

log_info(_Msg) ->
    ok.

mock_put(_Pid,_Bucket,Key) when is_integer(Key) ->
    {ok, Key};
mock_put(_Pid,_Bucket,_Key) ->
    {error,not_an_ineger}.


setup() ->
    meck:new(riakc_pb_socket, [passthrough]),
    meck:expect(riakc_pb_socket, start_link, fun(_Host, _Port) -> {ok, self()} end),
    meck:expect(riakc_pb_socket, put, fun(_Pid, _Obj) -> ok end),
    meck:expect(riakc_pb_socket, get, fun mock_put/3),
    {ok, package_registration_server:start_link()}.

teardown(_) ->
    meck:unload(riakc_pb_socket).

package_registration_tests() ->
    {
        setup,
        fun setup/0,
        fun teardown/1,
        [
            fun start_link_test/0,
            fun register_package_success_test/0,
            fun register_package_failure_test/0
        ]
    }.

start_link_test() ->
    {ok, Pid} = package_registration_server:start_link(),
    is_process_alive(Pid).

register_package_success_test() ->
    PackageId = 123,
    Result = package_registration_server:register_package(PackageId),
    ?assertMatch({ok, PackageId}, Result).

register_package_failure_test() ->
    %% Attempt to register a package, expecting an error tuple in response
    PackageId = something_else,
    Result = package_registration_server:register_package(PackageId),
    ?assertMatch({error, not_an_ineger}, Result).

% get_package_success_test() ->
%     %% Mock a successful GET request from Riak
%     meck:expect(riakc_pb_socket, get, fun(_ClientPid, _Bucket, _Key) ->
%         {ok, riakc_obj:new(<<"test_bucket">>, <<"123">>, <<"{\"locationId\":\"pending\"}">>)}
%     end),

%     %% Test get_package/1 with an expected JSON response
%     PackageId = 123,
%     Result = package_registration_server:get_package(PackageId),
    
%     %% Adjust ExpectedData to use map syntax with binary keys and values
%     ExpectedData = #{<<"locationId">> => <<"pending">>},
%     ?assertMatch({ok, ExpectedData}, Result).

% get_package_failure_test() ->
%     %% Mock a failure for GET request from Riak
%     meck:expect(riakc_pb_socket, get, fun(_ClientPid, _Bucket, _Key) ->
%         {error, notfound}
%     end),

%     %% Attempt to get a non-existing package, expecting an error
%     PackageId = 789,
%     Result = package_registration_server:get_package(PackageId),
%     ?assertMatch({error, notfound}, Result).

-endif.
