-module(notification_service).
-behaviour(gen_server).

%% API
-export([start_link/0, notify/2]).
-export([init/1, handle_cast/2, terminate/2]).

%% Starts the notification service
start_link() ->
    gen_server:start_link({local, notification_service}, ?MODULE, [], []).

%% Public API to send a notification (asynchronous)
notify(PackageId, {Status, Location}) ->
    gen_server:cast(notification_service, {notify, PackageId, Status, Location}).

%% gen_server Callbacks
callback_mode() ->
    handle_event_function.
%% Initialization of the service
init([]) ->
    io:format("Notification Service started~n"),
    {ok, #{}}.

%% Handle incoming notifications
handle_cast({notify, PackageId, Status, Location}, State) ->
    %% Simulate sending notification (e.g., logging to console or sending HTTP request)
    io:format("Notification: Package ~p status updated to ~p at location ~p~n", [PackageId, Status, Location]),
    
    %% For example, you can send HTTP requests, log to files, or send emails here
    %% HTTP notification example (you can replace this with real HTTP client code):
    %% httpc:request(post, {"http://example.com/notify", [], "application/json", JsonPayload}, [], []),

    {noreply, State}.

%% Termination
terminate(_Reason, _State) ->
    io:format("Notification Service shutting down~n"),
    ok.
