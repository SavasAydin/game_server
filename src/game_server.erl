-module(game_server).
-behaviour(gen_server).

-export([start_link/1]).
-export([init/1, 
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 code_change/3,
	 terminate/2
	]).

-record(state, {socket}). 

start_link(LitenSocket) ->
    gen_server:start_link(?MODULE, LitenSocket, []).
 
init(ListenSocket) ->
    gen_server:cast(self(), accept),
    {ok, #state{socket = ListenSocket}}.

handle_cast(accept, ListenSocket) ->
    {ok, AcceptSocket} = gen_tcp:accept(ListenSocket),
    game_server_sup:start_socket(), 
    {noreply, #state{socket = AcceptSocket}}.

handle_call(_E, _From, State) ->
    {noreply, State}.

handle_info(_, S) ->
    {noreply, S}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
 
terminate(normal, _State) ->
    ok.



