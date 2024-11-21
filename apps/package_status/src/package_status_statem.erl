-module(package_status_statem).
-behaviour(gen_statem).

%% API
-export([start_link/1, update_status/2, get_status/1, update_location/2]).

%% gen_statem callbacks
-export([init/1, callback_mode/0, handle_event/4, terminate/3, code_change/4]).

-define(RIAK_URL, "https://database.mertlymedia.net:443/buckets/test_bucket/keys/").

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
init([PackageId]) ->
    inets:start(), %% Start the inets application for HTTP requests
    InitialData = #{status => "pending", location => "unknown", package_id => PackageId},
    {ok, pending, InitialData}.

%% Handle cast events (update status)
handle_event(cast, {update_status, Event}, State, Data) ->
    NewState = case {State, Event} of
        {pending, start_delivery} -> in_transit;
        {in_transit, complete_delivery} -> delivered;
        _ -> State
    end,
    UpdatedData = maps:put(status, atom_to_list(NewState), Data),
    send_http_update(UpdatedData),
    {next_state, NewState, UpdatedData};

%% Handle cast events (update location)
handle_event(cast, {update_location, Location}, State, Data) ->
    UpdatedData = maps:put(location, Location, Data),
    send_http_update(UpdatedData),
    {next_state, State, UpdatedData};

%% Handle synchronous call to get status and location
handle_event({call, From}, get_status, State, Data) ->
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

%% Helper function to send HTTP updates
send_http_update(Data) ->
    PackageId = maps:get(package_id, Data),
    URL = ?RIAK_URL ++ integer_to_list(PackageId) ++ "?returnbody=true",
    Headers = [{"Content-Type", "application/json"}],
    Body = jsx:encode(Data),
    case httpc:request(put, {URL, Headers, "application/json", Body}, [], []) of
        {ok, {{_, 200, _}, _, _}} ->
            io:format("Successfully updated package: ~p~n", [PackageId]);
        {ok, {{_, StatusCode, _}, _, ResponseBody}} ->
            io:format("Failed to update package ~p: ~p~n", [PackageId, StatusCode]);
        {error, Reason} ->
            io:format("HTTP error while updating package ~p: ~p~n", [PackageId, Reason])
    end.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-endif.
