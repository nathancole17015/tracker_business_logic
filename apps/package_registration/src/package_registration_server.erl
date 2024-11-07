-module(package_registration_server).
-behaviour(gen_server).

%% API
-export([start_link/0, register_package/3, stop/0]).  % Add stop/0 to exports

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(RIAK_HOST, "database.mertlymedia.net").
-define(RIAK_PORT, 8087).

%% Define the state record with a field for the Riak connection PID
-record(state, {riak_pid}).

%% Public API
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

register_package(PackageId, Sender, Receiver) ->
    gen_server:call(?MODULE, {register, PackageId, Sender, Receiver}).

stop() ->
    gen_server:stop(?MODULE).

%% gen_server Callbacks

init([]) ->
    %% Connect to Riak when the server starts
    case riakc_pb_socket:start_link(?RIAK_HOST, ?RIAK_PORT) of
        {ok, RiakPid} ->
            {ok, #state{riak_pid = RiakPid}};
        {error, Reason} ->
            {stop, Reason}
    end.

handle_call({register, PackageId, Sender, Receiver}, _From, #state{riak_pid = RiakPid} = State) ->
    %% Create a new object to store in Riak
    Bucket = <<"package_registration">>,
    Key = integer_to_binary(PackageId),
    Value = term_to_binary({Sender, Receiver}),
    Obj = riakc_obj:new(Bucket, Key, Value),

    %% Store the object in Riak
    case riakc_pb_socket:put(RiakPid, Obj) of
        ok ->
            io:format("Package ~p registered from ~p to ~p~n", [PackageId, Sender, Receiver]),
            {reply, {ok, PackageId}, State};
        {error, Reason} ->
            io:format("Failed to register package: ~p~n", [Reason]),
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
