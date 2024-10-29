-module(riak_example).
-export([start/0, store_data/3, fetch_data/2, delete_data/2, stop/0]).

-record(state, {riak_pid}).

%% Start a connection to Riak
start() ->
    Host = "localhost",
    Port = 8087,
    case riakc_pb_socket:start_link(Host, Port) of
        {ok, RiakPid} ->
            register(riak_connection, RiakPid),
            {ok, RiakPid};
        {error, Reason} ->
            io:format("Failed to connect to Riak: ~p~n", [Reason]),
            {error, Reason}
    end.

%% Store data in Riak
store_data(Bucket, Key, Data) ->
    RiakPid = whereis(riak_connection),
    Value = term_to_binary(Data),
    Obj = riakc_obj:new(Bucket, Key, Value),
    riakc_pb_socket:put(RiakPid, Obj).

%% Fetch data from Riak
fetch_data(Bucket, Key) ->
    RiakPid = whereis(riak_connection),
    case riakc_pb_socket:get(RiakPid, Bucket, Key) of
        {ok, Obj} ->
            Value = binary_to_term(riakc_obj:get_value(Obj)),
            {ok, Value};
        {error, not_found} ->
            {error, not_found};
        {error, Reason} ->
            {error, Reason}
    end.

%% Delete data from Riak
delete_data(Bucket, Key) ->
    RiakPid = whereis(riak_connection),
    riakc_pb_socket:delete(RiakPid, Bucket, Key).

%% Stop the Riak connection
stop() ->
    RiakPid = whereis(riak_connection),
    riakc_pb_socket:stop(RiakPid),
    unregister(riak_connection).
