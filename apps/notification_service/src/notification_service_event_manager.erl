-module(notification_service_event_manager).
-behaviour(gen_server).

%% API
-export([start_link/0, notify/1, add_handler/1, remove_handler/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% Public API
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

notify(Message) ->
    gen_server:cast(?MODULE, {notify, Message}).

add_handler(Handler) ->
    gen_server:call(?MODULE, {add_handler, Handler}).

remove_handler(Handler) ->
    gen_server:call(?MODULE, {remove_handler, Handler}).

%% gen_server callbacks
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
