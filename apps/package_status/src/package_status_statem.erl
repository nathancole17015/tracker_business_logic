-module(package_status_statem).
-behaviour(gen_statem).

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
    gen_statem:call(RegisteredName, get_status).

%% Callback mode
callback_mode() ->
    handle_event_function.

%% Initialization: Start with an empty map (no initial status or location)
init([_PackageId]) ->
    {ok, pending, #{status => "pending", location => "unknown"}}.

%% Handle cast events (update status)
handle_event(cast, {update_status, start_delivery}, pending, Data) ->
    {next_state, in_transit, maps:put(status,"in_transit",Data)};

handle_event(cast, {update_status, complete_delivery}, in_transit, Data) ->
    {next_state, delivered, maps:put(status,"delivered",Data)};

%% Handle cast events (update location)
handle_event(cast, {update_location, Location}, State, Data) ->
    {next_state, State, maps:put(location,Location,Data)};

%% Handle synchronous call to get status and location
handle_event({call,From}, get_status, State, Data) ->
    gen_statem:reply(From, Data),
    {next_state, State, Data};

%% Catch-all clause for debugging any unexpected events
handle_event(EventType, EventContent, State, Data) ->
    io:format("Unhandled event ~p with content ~p. Current state: ~p, Data: ~p~n", [EventType, EventContent, State, Data]),
    {next_state, State, Data}.

terminate(_Reason, _State, _Data) ->
    io:format("State machine terminating~n", []),
    ok.

code_change(_OldVsn, State, Data, _Extra) ->
    {ok, State, Data}.


-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").
handle_event_test_() -> 
    [
        ?_assertEqual(
            {next_state, in_transit, #{status => "in_transit"}},
            handle_event(cast, {update_status, start_delivery}, pending, #{})
        ),

        ?_assertEqual(
            {next_state, delivered, #{status => "delivered"}},
            handle_event(cast, {update_status, complete_delivery}, in_transit, #{status => "in_transit"})
        ),

        ?_assertEqual(
            {next_state, in_transit, #{location => "Warehouse 42", status => "in_transit"}},
            handle_event(cast, {update_location, "Warehouse 42"}, in_transit, #{status => "in_transit"})
        ),

        ?_assertEqual(
            {next_state, in_transit, #{status => "in_transit", location => "Warehouse 42"}},
            handle_event(unexpected_type, unexpected_content, in_transit, #{status => "in_transit", location => "Warehouse 42"})
        )
    ].

api_test_() ->
    {ok, Pid} = package_status_statem:start_link(1),
    % Funs force assertions to wait on casts like update status to evaluate
    % before testing
    Status = fun () ->
        package_status_statem:update_status(1, start_delivery),
        gen_statem:call(Pid, get_status) end,
    Location = fun () ->
        package_status_statem:update_location(1, "Warehouse 42"),
        gen_statem:call(Pid, get_status) end,

    [
        ?_assertEqual(
            #{status => "pending", location => "unknown"},
            gen_statem:call(Pid, get_status)
        ),
        ?_assertEqual(
            #{status => "in_transit", location => "unknown"},
            Status()
        ),
        ?_assertEqual(
            #{status => "in_transit", location => "Warehouse 42"},
            Location()
        )
    ].

-endif.
