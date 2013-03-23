-module(msg_handler_tests).

-include_lib("eunit/include/eunit.hrl").

-record(account, {name, password}).

msg_handler_gen_server_start_stop_test() ->
    ?assertMatch({ok, _}, msg_handler:start_link()),
    [] = msg_handler:get_accounts(),
    ?assertEqual(ok, msg_handler:stop()).

handle_command_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
      fun register_account/0
     ]}.

register_account() ->
    Savas = create_account(savas, pass),
    ?assertEqual([], msg_handler:get_accounts()),
    ?assertEqual(registered, msg_handler:register(Savas)),
    ?assertEqual(Savas, msg_handler:get_accounts()).
    
create_account(Name, Password) ->
    #account{name = Name, password = Password}.

setup() ->
    {ok, Pid} = msg_handler:start_link(),
    Pid.

cleanup(_) ->
    ok = msg_handler:stop().
