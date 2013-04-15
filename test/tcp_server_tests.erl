-module(tcp_server_tests).

-include_lib("eunit/include/eunit.hrl").

receive_message_send_ack_test_() ->
    {foreach,
     setup,
     fun setup/0,
     fun cleanup/1,
     [
      fun connect_to_tcp_server/0
     ]
    }.

connect_to_tcp_server() ->
    ?assertEqual(ok, assert_tcp_server_is_listenning(8080)).

assert_tcp_server_is_listenning(Port) ->
    {ok, Sock} = connect(Port, [{active,false},{packet,2}]),
    ok = close(Sock).

close(Sock) ->
    gen_tcp:close(Sock).

connect(Port, TcpOptions) ->
    gen_tcp:connect("localhost", Port, TcpOptions).

setup() ->
    {ok, Pid} = tcp_server:start_link({message_handler, start_handler}),
    Pid.

cleanup(_) ->
    tcp_server:stop().



