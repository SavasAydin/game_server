-module(game_server).

-export([
	 start/0
	 ,stop/0
	]).

-export([
	 listen/1
	 ,accept_worker/2
	]).

-define(TCP_OPTIONS, [binary, {reuseaddr, true},{packet, 0}, {active, false}]).
-define(PORT, 8080).

%% Client functions
start() ->
    mnesia:start(),
    mnesia:wait_for_tables([account], 1000),
    start(?PORT).

stop()->
    mnesia:stop(),
    {ok,Socket} = connect("localhost",?PORT,?TCP_OPTIONS),
    ok = gen_tcp:send(Socket, "stop"),
    "stopping" = recv(Socket).

%% server functions    
start(Port) ->
    spawn_link(?MODULE, listen, [Port]),
    ok.

listen(Port) ->
    case gen_tcp:listen(Port, [binary, {reuseaddr, true},
			       {packet, 0}, {active, false}]) of
	{ok, ListenSock} ->
	    spawn(?MODULE, accept_worker, [self(), ListenSock]),
	    loop(ListenSock);
	{error, Error} ->
	    {error, Error};
	Other ->
	    Other
    end.

loop(ListenSocket) ->
    receive
	next_worker ->
	    spawn_link(?MODULE, accept_worker, [self(), ListenSocket]);
	stop ->
	    gen_tcp:close(ListenSocket),
	    ok
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


%% private functions
analyze(Data)->
    Command = gameLib:decode(Data),
    game:handle_command(Command).

send(Socket, Data)->
    gen_tcp:send(Socket, gameLib:encode(Data)). 

recv(Socket)->
    {ok, Received} = gen_tcp:recv(Socket, 0),
    gameLib:decode(Received).

connect(Host, Port, Options)->
    gen_tcp:connect(Host, Port, Options).

close(Socket)->
    gen_tcp:close(Socket).
