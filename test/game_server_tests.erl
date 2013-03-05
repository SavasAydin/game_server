-module(game_server_tests).
-include_lib("eunit/include/eunit.hrl").

server_start_and_stop_test() ->
    ?assertMatch({ok, _}, game_server:start_link()),
    {ok, Sock} = gen_tcp:connect("localhost", 8080,[{active,false},{packet,2}]),
    gen_tcp:close(Sock),
    ?assertEqual(ok, game_server:stop()),
    {error, econnrefused} = gen_tcp:connect("localhost", 8080,[{active,false},{packet,2}]).

server_receives_msg_test() ->
    Msg = "hello!",
       game_server:start_link(),
    {ok, Sock} = gen_tcp:connect("localhost", 8080,[{active,false},{packet,2}]),
    send(Sock, Msg),
    ?assertEqual(Msg, recv(Sock)),
    gen_tcp:close(Sock),
    game_server:stop().
    
send(Sock, Msg) ->    
    gen_tcp:send(Sock,Msg).

recv(Sock) ->
    {ok, A} = gen_tcp:recv(Sock,0),
        A.

