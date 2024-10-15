-module(notification_service).
-behaviour(application).
-behaviour(supervisor).
-behaviour(gen_server).
-behaviour(gen_event).

%% API
-export([start/2, stop/1]).
-export([start_link/0, notify/1, add_handler/1, remove_handler/1]).

%% Application callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% gen_event callbacks
-export([init/1, handle_event/2, handle_info/2, terminate/2, code_change/3]).

%% Application Entry
start(_StartType, _StartArgs) ->
    notification_service:start_link().

stop(_State) ->
    ok.

%% Supervisor API
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% Supervisor init
init([]) ->
    SupFlags = #{strategy => one_for_one, intensity => 1, period => 5},
    ChildSpecs = [
        #{id => event_manager,
          start => {notification_service, start_link_event_manager, []},
          restart => permanent,
          shutdown => 5000,
          type => worker
        }
    ],
    {ok, {SupFlags, ChildSpecs}}.

%% gen_server API for Event Manager
start_link_event_manager() ->
    gen_server:start_link({local, event_manager}, ?MODULE, [], []).

notify(Message) ->
    gen_server:cast(event_manager, {notify, Message}).

add_handler(Handler) ->
    gen_server:call(event_manager, {add_handler, Handler}).

remove_handler(Handler) ->
    gen_server:call(event_manager, {remove_handler, Handler}).

%% gen_server Callbacks
init([]) ->
    {ok, EventManager} = gen_event:start_link(),
    {ok, #{event_manager => EventManager}}.

handle_call({add_handler, Handler}, _From, State) ->
    gen_event:add_handler(State#{"event_manager"}, Handler, []),
    {reply, ok, State};

handle_call({remove_handler, Handler}, _From, State) ->
    gen_event:delete_handler(State#{"event_manager"}, Handler, []),
    {reply, ok, State};

handle_call(_Request, _From, State) ->
    {reply, error, State}.

handle_cast({notify, Message}, State) ->
    gen_event:notify(State#{"event_manager"}, Message),
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% gen_event Callbacks for Notification Handler
init([]) ->
    {ok, []}.

handle_event(Message, State) ->
    io:format("Notification: ~p~n", [Message]),
    {ok, State}.

handle_info(_Info, State) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
