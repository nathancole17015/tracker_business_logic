-module(package_status_statem).
-behaviour(gen_statem).

%% API
-export([start_link/1, update_status/2, get_status/1, update_location/2]).

%% gen_statem callbacks
-export([init/1, callback_mode/0, handle_event/4, terminate/3, code_change/4]).

%% Start the state machine and register it with the package ID
start_link(PackageId) ->
    RegisteredName = list_to_atom("package_status_" ++ integer_to_list(PackageId)),
    gen_statem:start_link({local, RegisteredName}, ?MODULE, [], []).

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
    gen_statem:call(RegisteredName, get_status).

%% Callback mode
callback_mode() ->
    handle_event_function.

%% Initialization: Start with an empty map (no initial status or location)
init([]) ->
    io:format("Starting state machine. Initial state: pending~n"),
    {ok, pending, #{status => "pending", location => "unknown"}}.

%% Handle cast events (update status)
handle_event(cast, {update_status, start_delivery}, pending, Data) ->
    io:format("Package is now in transit.~n"),
    NewData = Data#{status => "in_transit"},
    {next_state, in_transit, NewData};

handle_event(cast, {update_status, complete_delivery}, in_transit, Data) ->
    io:format("Package has been delivered.~n"),
    NewData = Data#{status => "delivered"},
    {next_state, delivered, NewData};

%% Handle cast events (update location)
handle_event(cast, {update_location, Location}, _StateName, Data) ->
    io:format("Package location updated to: ~p~n", [Location]),
    NewData = Data#{location => Location},
    {keep_state, NewData};

%% Handle synchronous call to get status and location
handle_event(call, get_status, _StateName, Data) ->
    Status = maps:get(status, Data),
    Location = maps:get(location, Data),
    {reply, {ok, Status, Location}, _StateName, Data};

%% Fallback for unknown events
handle_event(_EventType, _EventContent, State, Data) ->
    io:format("Received an unknown event. Current state: ~p~n", [State]),
    {keep_state_and_data, Data}.

terminate(_Reason, _State, _Data) ->
    ok.

code_change(_OldVsn, State, Data, _Extra) ->
    {ok, State, Data}.
