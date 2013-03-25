-module(msg_handler_tests).

-include_lib("eunit/include/eunit.hrl").

msg_handler_gen_server_start_stop_test() ->
    ?assertMatch({ok, _}, msg_handler:start_link()),
    ?assertEqual(ok, msg_handler:stop()).

