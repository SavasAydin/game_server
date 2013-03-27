-module(msg_handler_tests).

-include_lib("eunit/include/eunit.hrl").

-include("include/account.hrl").

msg_handler_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
      fun register_account/0
     ]}.

register_account() ->
    Savas = create_account(savas, pass),
    ?assertEqual(ok, msg_handler:register(Savas)).

setup() ->
    ok = mnesia:start(),
    {ok, Pid} = msg_handler:start_link(),
    Pid.

cleanup(_Pid) ->
    mnesia:stop(),
    ok = msg_handler:stop().

create_account(Name, Password) ->
    #account{name = Name, password = Password}.

