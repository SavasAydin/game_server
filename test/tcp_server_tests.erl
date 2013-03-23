-module(tcp_server_tests).

-include_lib("eunit/include/eunit.hrl").

receive_message_send_ack_test_() ->
    {foreach,
     setup,
     fun setup/0,
     fun cleanup/1,
     [
      fun send_and_receive_hello/0
     ]
    }.

send_and_receive_hello() ->
    {ok, Sock} = connect(8080, [{active,false},{packet,2}]),
    ok = send(Sock, "hello"),
    ?assertEqual("hello", recv(Sock)),
    ok = close(Sock).

send(Sock, Msg) ->    
    gen_tcp:send(Sock, Msg).

recv(Sock) ->
    {ok, A} = gen_tcp:recv(Sock, 0),
    A.

close(Sock) ->
    gen_tcp:close(Sock).

connect(Port, TcpOptions) ->
    gen_tcp:connect("localhost", Port, TcpOptions).
    
setup() ->
    {ok, Pid} = tcp_server_sup:start_link(),
    Pid.

cleanup(_) ->
    tcp_server_sup:stop().

    
