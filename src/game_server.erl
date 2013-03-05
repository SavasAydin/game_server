-module(game_server).
-behaviour(gen_server).

-export([start_link/0,
	 stop/0
	]).
-export([init/1, 
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 code_change/3,
	 terminate/2
	]).

-define(PORT, 8080).
-define(TCP_OPTIONS, [{active, once}, {packet, 0}]).

-record(state, {lsocket,
		asocket,
		socket,
		msg
	       }). 

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
    gen_server:cast(?MODULE, stop).

init([]) ->
    {ok, ListenSocket} = gen_tcp:listen(?PORT, ?TCP_OPTIONS),
    gen_server:cast(self(), accept),
    {ok, #state{lsocket = ListenSocket}}.

handle_cast(accept, State) ->
    {ok, AcceptSocket} = gen_tcp:accept(State#state.lsocket),
    {noreply, #state{asocket = AcceptSocket}};
handle_cast(stop, State) ->
    {stop, normal, State}.

handle_call(_E, _From, State) ->
    {reply, ok, State}.

handle_info({tcp, Socket, Data}, State) ->
    send(Socket, Data),
    {noreply, State#state{socket = Socket, msg = Data}};
handle_info({tcp_closed, _Socket, _}, State) ->
    {stop, normal, State};
handle_info({tcp_error, _Socket, _}, State) ->
    {stop, normal, State};
handle_info(_, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
 
terminate(normal, _State) ->
    ok.

send(Sock, Data) ->
    gen_tcp:send(Sock, Data).

