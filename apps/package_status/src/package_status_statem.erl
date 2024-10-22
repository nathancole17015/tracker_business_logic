-module(package_status_statem).
-behaviour(gen_statem).

%% API
-export([start_link/0, update_status/2, get_status/1]).

%% gen_statem callbacks
-export([init/1, handle_event/4, terminate/3, code_change/4]).

%% Public API
start_link() ->
    gen_statem:start_link({local, ?MODULE}, ?MODULE, [], []).

%% Update status (asynchronous)
update_status(PackageId, Event) ->
    gen_statem:cast(?MODULE, {update_status, PackageId, Event}).

%% Get status (synchronous)
get_status(PackageId) ->
    gen_statem:call(?MODULE, {get_status, PackageId}).

%% gen_statem Callbacks

%% Initialization: Start in the 'pending' state
init([]) ->
    io:format("Package Status State Machine started. Initial state: pending~n"),
    {ok, pending, #{}}.

%% Handle both casts and calls
handle_event(cast, {update_status, PackageId, start_delivery}, pending, Data) ->
    io:format("Package ~p is now in transit.~n", [PackageId]),
    NewData = Data#{PackageId => "in_transit"},
    {next_state, in_transit, NewData};

handle_event(cast, {update_status, PackageId, complete_delivery}, in_transit, Data) ->
    io:format("Package ~p has been delivered.~n", [PackageId]),
    NewData = Data#{PackageId => "delivered"},
    {next_state, delivered, NewData};

handle_event(cast, {update_status, _PackageId, _Event}, delivered, Data) ->
    io:format("Package is already delivered, no further updates possible~n"),
    {keep_state_and_data, Data};

%% New: Handle synchronous call to get package status
handle_event(call, {get_status, PackageId}, StateName, Data) ->
    io:format("Fetching current state for package ~p.~n", [PackageId]),
    case maps:get(PackageId, Data, undefined) of
        undefined -> {reply, {error, not_found}, StateName, Data};
        Status -> {reply, {ok, Status}, StateName, Data}
    end;

%% Fallback for unknown events
handle_event(_EventType, _EventContent, State, Data) ->
    io:format("Received an unknown event. Current state: ~p~n", [State]),
    {keep_state_and_data, Data}.

terminate(_Reason, _State, _Data) ->
    ok.

code_change(_OldVsn, State, Data, _Extra) ->
    {ok, State, Data}.
