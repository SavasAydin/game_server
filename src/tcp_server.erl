-module(tcp_server).

-export([start_link/0, stop/1]).
-export([acceptor/2, init/1]).

-include("include/account.hrl").

start_link() ->
    proc_lib:start_link(?MODULE, init, [self()]).

stop(Pid) ->
    Pid ! stop.

init(Parent) ->
    process_flag(trap_exit, true),
    register(?MODULE, self()),
    {ok, ListenSocket} = gen_tcp:listen(?PORT, ?TCP_OPTIONS),
    proc_lib:init_ack(Parent, {ok, self()}),
    acceptor(Parent, ListenSocket).

acceptor(_Parent,ListenSocket) ->
    {ok, Socket} = gen_tcp:accept(ListenSocket),
    Instance = term_to_binary(Socket),
    msg_handler:start_link(Instance),
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
