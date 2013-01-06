-module(game_server_tests).

-include_lib("eunit/include/eunit.hrl").

-define(TCP_OPTIONS, [binary, {reuseaddr, true},{packet, 0}, {active, false}]).
-define(PORT, 8080).

game_server_start_stop_test()->
    ?assertMatch(ok,game_server:start()),
    {ok,Socket} = gen_tcp:connect("localhost",?PORT,?TCP_OPTIONS),
    ok = gen_tcp:close(Socket),
    ?assertMatch("stopping", game_server:stop()),
    {error, econnrefused} = gen_tcp:connect("localhost",?PORT,?TCP_OPTIONS). 

game_server_send_command_recv_reply_test_()->
    {foreach,
     fun start/0,
     fun stop/1,
     [
      {"send undefined command, recv term()->no_command_found"
       ,fun undefined_command/0}
      ,{"send a register account command, recv term()->registered"
	,fun register_an_account/0}
      ,{"send re-register same account command, recv term()->already_registered"
	,fun re_register_same_account/0}
      ,{"send a list_accounts command, recv ListOfNames"
	,fun list_accounts/0}
      ,{"send a deregister account command, recv ok, deregistered"
	,fun deregister_command/0}
      ,{"send deregister unregistered account command, recv term()-> not_found"
	,fun deregister_unknown_account/0}
      ,{"send again list_accounts command, recv ListOfNames=[]"
	,fun list_accounts_again/0}
     ]}.


undefined_command()->
    UndefinedCommand = "undefined_command",
    ?assertEqual("error, no_command_found", send_recv_command(UndefinedCommand)).
    
register_an_account()->
    Name = "name",
    Pass = "pass",
    RegisterCommand = {"register", Name, Pass}, 
    Reply = "ok, registered",
    ?assertEqual(Reply, send_recv_command(RegisterCommand)).

re_register_same_account()->
    Name = "name",
    Pass = "whatever",
    RegisterCommand = {"register", Name, Pass}, 
    Reply = "error, already_registered",
    ?assertEqual(Reply, send_recv_command(RegisterCommand)).
    
list_accounts()->
    RegisteredName = "name",
    AnotherName = "another_name",
    AnotherPass = "another_pass",
    RegisterCommand = {"register", AnotherName, AnotherPass}, 
    "ok, registered" =  send_recv_command(RegisterCommand),    
    ListAccountsCommand = {"list_accounts"},
    ListOfNames = [RegisteredName, AnotherName],
    ?assertEqual(ListOfNames, send_recv_command(ListAccountsCommand)).
    
deregister_command()->
    RegisteredName = "name",
    AnotherName = "another_name",
    DeregisterCommand = {"deregister", RegisteredName},  
    DeregisterAnotherCommand = {"deregister", AnotherName},
    ?assertEqual("ok, deregistered", send_recv_command(DeregisterCommand)),
    ?assertEqual("ok, deregistered", send_recv_command(DeregisterAnotherCommand)).

deregister_unknown_account()->
    UnknownName = "unknown_name",
    DeregisterCommand = {"deregister", UnknownName},
    Reply = "error, not_found",
    ?assertEqual(Reply, send_recv_command(DeregisterCommand)).

list_accounts_again()->
    ListAccountsCommand = {"list_accounts"},
    ListOfNames = [],
    ?assertEqual(ListOfNames, send_recv_command(ListAccountsCommand)).
  
  
%% 
send_recv_command(Command)->
    BinCommand = gameLib:encode(Command),
    {ok,Socket} = gen_tcp:connect("localhost",?PORT,?TCP_OPTIONS),
    ok = gen_tcp:send(Socket, BinCommand),
    {ok, Reply} = gen_tcp:recv(Socket, 0),
    gameLib:decode(Reply).


start()->
    ok = game_server:start().

stop(_Pid)->
    "stopping" = game_server:stop().
