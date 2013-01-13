-module(game_server).

-export([start/0
	 ,stop/0
	 ,send_recv/2
	]).

-export([listen/2
	 ,accept_worker/2
	]).

-define(TCP_OPTIONS, [binary, {reuseaddr, true},{packet, 0}, {active, false}]).
-define(PORT, 8080).

start() ->
    mnesia:start(),
    mnesia:wait_for_tables([account], 1000),
    start(?PORT, ?TCP_OPTIONS).

send_recv(Socket, Data)->
    send(Socket,Data),
    recv(Socket).

stop()->
    mnesia:stop(),
    {ok,Socket} = connect("localhost",?PORT,?TCP_OPTIONS),
    ok = gen_tcp:send(Socket, "stop"),
    "stopping" = recv(Socket).

%% 
start(Port, TcpOpt) ->
    spawn(?MODULE, listen, [Port, TcpOpt]),
    ok.

listen(Port, TcpOpt) ->
    case gen_tcp:listen(Port, TcpOpt) of 			       
	{ok, ListenSock} ->
	    spawn(?MODULE, accept_worker, [self(), ListenSock]),
	    loop(ListenSock);
	{error, Error} ->
	    {error, Error}
    end.

loop(ListenSocket) ->
    receive
	next_worker ->
	    spawn_link(?MODULE, accept_worker, [self(), ListenSocket]);
	stop ->
	    gen_tcp:close(ListenSocket)	
    end,
    loop(ListenSocket).

accept_worker(Self, ListenSocket) ->
    case gen_tcp:accept(ListenSocket) of
	{ok, AcceptSocket} ->
	    Self ! next_worker,
	    handle(Self, AcceptSocket);
	{error, Reason} ->
	    {error, Reason}
    end.

handle(Self, AcceptSocket)->
        inet:setopts(AcceptSocket, [{active, once}]),
    receive
	{tcp, Socket, <<"stop">>} ->
	    send(Socket, "stopping"),
	    close(Socket),
	    close(AcceptSocket),
	    Self ! stop;
	{tcp, Socket, Data} ->
	    Reply = analyze(Data),
	    send(Socket, Reply),
	    handle(Self, Socket);
	{tcp_closed, Socket}->
	    {closed, Socket};
	{tcp_error, _Socket, Reason} ->
	    {error, Reason}
    end.

%% 
analyze(Data)->
    Command = gameLib:decode(Data),
    game:handle_command(Command).

send(Socket, Data)->
    case gen_tcp:send(Socket, gameLib:encode(Data)) of
	ok ->
	    ok;
	{error, Error} ->
	    {error, Error}
    end.

recv(Socket)->
    case gen_tcp:recv(Socket, 0) of
	{ok, Received} ->
	    gameLib:decode(Received);
	Error ->
	    Error
    end. 

connect(Host, Port, Options)->
    gen_tcp:connect(Host, Port, Options).

close(Socket)->
    gen_tcp:close(Socket).
