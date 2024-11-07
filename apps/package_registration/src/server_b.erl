-module(server_b).
-behaviour(gen_server).

%% API
-export([start_link/0, send_message/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, terminate/2]).

%% Start the server
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% Public API for sending a message to server_a on another node
send_message(Msg) ->
    %% Assuming server_a is running on node `server_a@computer1`
    gen_server:cast({global, {server_a, 'server_a@computer1'}}, {msg_from_b, Msg}).

init([]) ->
    {ok, #{}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({msg_from_a, Msg}, State) ->
    io:format("Received message from server_a: ~p~n", [Msg]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.
