-module(game_server_tests).

-include_lib("eunit/include/eunit.hrl").

-define(TCP_OPTIONS, [binary, {reuseaddr, true},{packet, 0}, {active, false}]).
-define(PORT, 8080).

game_server_start_test()->
    ?assertMatch(ok,game_server:start()),
    {ok,Socket} = gen_tcp:connect("localhost",?PORT,?TCP_OPTIONS),
    ok = gen_tcp:close(Socket).
 
game_server_stop_test()->
    ?assertMatch("stopping", game_server:stop()),
    {error, econnrefused} = gen_tcp:connect("localhost",?PORT,?TCP_OPTIONS). 

