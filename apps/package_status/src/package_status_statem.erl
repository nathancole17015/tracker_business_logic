-module(package_status_statem).
-behaviour(gen_statem).

%% Macro to define the atom for get_status
-define(GET_STATUS, get_status).

%% API
-export([start_link/1, update_status/2, get_status/1, update_location/2]).

%% gen_statem callbacks
-export([init/1, callback_mode/0, handle_event/4, terminate/3, code_change/4]).

%% Start the state machine and register it with the package ID
start_link(PackageId) ->
    RegisteredName = list_to_atom("package_status_" ++ integer_to_list(PackageId)),
    io:format("Registering process with name: ~p~n", [RegisteredName]),
    gen_statem:start_link({local, RegisteredName}, ?MODULE, [PackageId], []).

%% API: Asynchronous update of the package status
update_status(PackageId, Event) ->
    RegisteredName = list_to_atom("package_status_" ++ integer_to_list(PackageId)),
    gen_statem:cast(RegisteredName, {update_status, Event}).

%% API: Asynchronous update of the package location
update_location(PackageId, Location) ->
    RegisteredName = list_to_atom("package_status_" ++ integer_to_list(PackageId)),
    gen_statem:cast(RegisteredName, {update_location, Location}).

%% API: Synchronous get the status and location of a package
get_status(PackageId) ->
    RegisteredName = list_to_atom("package_status_" ++ integer_to_list(PackageId)),
    gen_statem:call(RegisteredName, ?GET_STATUS).

%% Callback mode
callback_mode() ->
    io:format("Callback mode: handle_event_function~n"),
    handle_event_function.

%% Initialization: Start with an empty map (no initial status or location)
init([PackageId]) ->
    io:format("Starting state machine for package ~p. Initial state: pending~n", [PackageId]),
    {ok, pending, #{status => "pending", location => "unknown"}}.

%% Handle cast events (update status)
handle_event(cast, {update_status, start_delivery}, pending, Data) ->
    io:format("Event: start_delivery, Current state: pending. Data: ~p~n", [Data]),
    NewData = Data#{status => "in_transit"},
    io:format("Package is now in transit.~nNext state: in_transit, New Data: ~p~n", [NewData]),
    {next_state, in_transit, NewData};

handle_event(cast, {update_status, complete_delivery}, in_transit, Data) ->
    io:format("Event: complete_delivery, Current state: in_transit. Data: ~p~n", [Data]),
    NewData = Data#{status => "delivered"},
    io:format("Package has been delivered.~nNext state: delivered, New Data: ~p~n", [NewData]),
    {next_state, delivered, NewData};

%% Handle cast events (update location)
handle_event(cast, {update_location, Location}, _StateName, Data) ->
    io:format("Event: update_location, Current state: ~p, Location: ~p~n", [_StateName, Location]),
    NewData = Data#{location => Location},
    io:format("Package location updated to: ~p. New Data: ~p~n", [Location, NewData]),
    {keep_state, NewData};

%% Handle synchronous call to get status and location
handle_event(call, ?GET_STATUS, StateName, Data) ->
    io:format("Handling call event: get_status, Current state: ~p, Data: ~p~n", [StateName, Data]),
    Status = maps:get(status, Data),
    Location = maps:get(location, Data),
    io:format("Replying with Status: ~p, Location: ~p~n", [Status, Location]),
    {keep_state_and_data, {reply, {ok, Status, Location}}};

%% Catch-all clause for debugging any unexpected events
handle_event(EventType, EventContent, State, Data) ->
    io:format("Unhandled event ~p with content ~p. Current state: ~p, Data: ~p~n", [EventType, EventContent, State, Data]),
    {keep_state_and_data, Data}.

terminate(_Reason, _State, _Data) ->
    io:format("State machine terminating~n", []),
    ok.

code_change(_OldVsn, State, Data, _Extra) ->
    {ok, State, Data}.
