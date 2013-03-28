-module(msg_handler_tests).

-include_lib("eunit/include/eunit.hrl").

-include("include/account.hrl").

msg_handler_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
      fun register_account/0,
      fun deregister_account/0
     ]}.

register_account() ->
    Savas = create_account(savas, pass),
    [] =  msg_handler:get_accounts(),
    ?assertEqual(ok, msg_handler:register_account(Savas)),
    ?assertEqual([Savas], msg_handler:get_accounts()).

deregister_account() ->
    Savas = create_account(savas, pass),
    [Savas] =  msg_handler:get_accounts(),
    ?assertEqual(ok, msg_handler:deregister_account(Savas)),
    [] =  msg_handler:get_accounts().

setup() ->
    ok = mnesia:start(),
    {ok, Pid} = msg_handler:start_link(),
    Pid.

cleanup(_Pid) ->
    mnesia:stop(),
    ok = msg_handler:stop().

create_account(Name, Password) ->
    #account{name = Name, password = Password}.

