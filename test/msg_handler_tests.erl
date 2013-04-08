-module(msg_handler_tests).

-include_lib("eunit/include/eunit.hrl").

-include("include/account.hrl").

msg_handler_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
      {"registers non existing account, returns ok",
       perform_commands(ok, register_account, savas())},
      {"lists account table, returns [Savas]",
       perform_commands([savas()], get_accounts, no_args)},
      {"registers existing name with different password, returns error",
       perform_commands({error, already_exist}, register_account, savas_different_password())},
      {"lists accounts after re-register with same name, returns [Savas]",
       perform_commands([savas()], get_accounts, no_args)},
      {"registers another non existing account, returns ok",
       perform_commands(ok, register_account, gianfranco())},
      {"lists account table, returns [Savas, Gianfranco]",
       perform_commands([savas(), gianfranco()], get_accounts, no_args)},
      {"deregisters existing account, returns ok",
       perform_commands(ok, deregister_account, gianfranco())},
      {"lists accounts after Gianfranco is removed, returns [Savas]",
       perform_commands([savas()], get_accounts, no_args)},
      {"deregisters non existing account, returns error",
       perform_commands({error, not_exist}, deregister_account, simon())},
      {"gets existing account by name, returns [Savas]",
       perform_commands([savas()], get_account, savas)},
      {"deregisters Savas, returns ok",
       perform_commands(ok, deregister_account, savas())},
      {"lists account table, returns empty list",
       perform_commands([], get_accounts, no_args)},
      {"gets non existing account by name, returns error",
       perform_commands({error, not_exist}, get_account, savas)}
     ]}.

perform_commands(ReturnValue, Fun, no_args) ->
    fun() ->
	    ?assertEqual(ReturnValue, msg_handler:Fun())
    end;
perform_commands(ReturnValue, Fun, Args) ->
    fun() ->
	    ?assertEqual(ReturnValue, msg_handler:Fun(Args))
    end.

setup() ->
    ok = mnesia:start(),
    {ok, Pid} = msg_handler:start_link(),
    Pid.

cleanup(_) ->
    mnesia:stop(),
    ok = msg_handler:stop().

savas() ->
    #account{name = savas, password = pass}.

savas_different_password() ->
    #account{name = savas, password = another_pass}.

gianfranco() ->
    #account{name = gianfranco, password = pass}.

simon() ->
    #account{name = simon, password = pass}.

