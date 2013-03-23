-module(tcp_server_SUITE).
-include_lib("common_test/include/ct.hrl").

-export([all/0,
	 init_per_suite/1,
	 end_per_suite/1
	]).
-export([receive_msg_send_ack/1]).

all() ->
    [receive_msg_send_ack].
      
init_per_suite(Config) ->
    application:start(tcp_server),
    Sock = connect(8080, [{active,false},{packet,2}]),
    [{socket, Sock} | Config].

end_per_suite(_Config) ->
    application:stop(tcp_server).

receive_msg_send_ack(Config) ->
    Socket = ?config(socket, Config),
    send(Socket, "hello"),
    "hello" = recv(Socket),
    send(Socket, "hello_again"),
    "hello_again" = recv(Socket),
    close(Socket).

connect(Port, TcpOptions)->
    {ok, Sock} = gen_tcp:connect("localhost", Port, TcpOptions),
    Sock.

close(Sock) ->
    ok = gen_tcp:close(Sock).

send(Sock, Msg) ->    
    ok = gen_tcp:send(Sock,Msg).

recv(Sock) ->
    {ok, A} = gen_tcp:recv(Sock,0),
    A.
