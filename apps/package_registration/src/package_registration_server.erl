-module(package_registration_server).
-behaviour(gen_server).

%% API
-export([start_link/0, register_package/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(RIAK_HOST, "localhost").
-define(RIAK_PORT, 8087).

%% Public API
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

register_package(PackageId, Sender, Receiver) ->
    gen_server:call(?MODULE, {register, PackageId, Sender, Receiver}).

%% gen_server Callbacks

init([]) ->
    %% Initialize state (could be a Riak connection here)
    {ok, #{}}.

handle_call({register, PackageId, Sender, Receiver}, _From, State) ->
    %% Simulate storing package info in Riak (a map in this case)
    io:format("Registering package ~p from ~p to ~p~n", [PackageId, Sender, Receiver]),
    NewState = State#{PackageId => {Sender, Receiver}},
    {reply, {ok, PackageId}, NewState}.

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
