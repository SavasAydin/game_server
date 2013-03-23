-module(tcp_server).

-export([start_link/0, stop/0]).
-export([acceptor/2, init/1]).

-define(PORT, 8080).
-define(TCP_OPTIONS, [binary, {packet, 0}, {active, once}]).
 
start_link() ->
    proc_lib:start_link(?MODULE, init, [self()]).

stop() ->
     ?MODULE ! stop.

init(Parent) ->
    process_flag(trap_exit, true),
    register(?MODULE, self()),
    {ok, ListenSocket} = gen_tcp:listen(?PORT, ?TCP_OPTIONS),
    proc_lib:init_ack(Parent, {ok, self()}),
    acceptor(Parent, ListenSocket).

acceptor(Parent,ListenSocket) ->
    {ok, Socket} = gen_tcp:accept(ListenSocket),
    tcp_server_sup:start_child(Parent),
    loop(Socket).

loop(Socket) ->
    inet:setopts(Socket, [binary, {nodelay, true}, {active, once}]),
    receive
	{tcp, Socket, Data} ->    
	    gen_tcp:send(Socket, Data),
	    loop(Socket);
	{tcp_closed, Socket} ->
	    {error, disconnected};
	stop -> 
	    terminate(Socket, normal)
    end.

terminate(_, Reason) ->
    exit(Reason).
