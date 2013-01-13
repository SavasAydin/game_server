-module(game_server_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([all/0
	 ,init_per_suite/1 
	 ,init_per_testcase/2
	 ,end_per_testcase/2
	 ,end_per_suite/1
	]).
-export([register_nonexisting_account/1
 	 ,try_register_existing_account/1
	 ,deregister_existing_account/1
	 ,try_deregister_nonexisting_account/1
	 ,try_undefined_command/1
	]).

-define(TCP_OPTIONS, [binary, {reuseaddr, true},{packet, 0}, {active, false}]).
-define(PORT, 8080).

all()->
    [register_nonexisting_account
     ,try_register_existing_account
     ,deregister_existing_account
     ,try_deregister_nonexisting_account
     ,try_undefined_command].
    
init_per_suite(Config)->
    db_server:init(),
    ok = game_server:start(),
    Name = "name",
    Pass = "pass",
    RegisterCommand = {"register", Name, Pass}, 
    DeregisterCommand = {"deregister", Name}, 
    ListAccountsCommand = {"list_accounts"},
    UndefinedCommand = {"undefined_command"},
    [{register_command, RegisterCommand}
     ,{deregister_command, DeregisterCommand}
     ,{undefined_command, UndefinedCommand}
     ,{list_accounts, ListAccountsCommand} | Config].

end_per_suite(_Config)->
    game_server:stop().

init_per_testcase(register_nonexisting_account, Config)->
    {ok,Socket} = gen_tcp:connect("localhost",?PORT,?TCP_OPTIONS),
    [{socket, Socket} | Config];
init_per_testcase(try_register_existing_account, Config)->
    {ok,Socket} = gen_tcp:connect("localhost",?PORT,?TCP_OPTIONS),
    [{socket, Socket} | Config];
init_per_testcase(deregister_existing_account, Config)->
    {ok,Socket} = gen_tcp:connect("localhost",?PORT,?TCP_OPTIONS),
    [{socket, Socket} | Config];
init_per_testcase(try_deregister_nonexisting_account, Config)->
    {ok,Socket} = gen_tcp:connect("localhost",?PORT,?TCP_OPTIONS),
    [{socket, Socket} | Config];
init_per_testcase(try_undefined_command, Config)->
    {ok,Socket} = gen_tcp:connect("localhost",?PORT,?TCP_OPTIONS),
    [{socket, Socket} | Config].

end_per_testcase(register_nonexisting_account, Config)->
    Socket = ?config(socket, Config),
    gen_tcp:close(Socket);
end_per_testcase(try_register_existing_account, Config)->
    Socket = ?config(socket, Config),
    gen_tcp:close(Socket);
end_per_testcase(deregister_existing_account, Config)->
    Socket = ?config(socket, Config),
    gen_tcp:close(Socket);
end_per_testcase(try_deregister_nonexisting_account, Config)->
    Socket = ?config(socket, Config),
    gen_tcp:close(Socket);
end_per_testcase(try_undefined_command, Config)->
    Socket = ?config(socket, Config),
    gen_tcp:close(Socket).

register_nonexisting_account(Config)->
    RegisterCom = ?config(register_command, Config),
    ListAccCom = ?config(list_accounts, Config),
    Socket = ?config(socket, Config),
    [] = game_server:send_recv(Socket, ListAccCom),
    "ok, registered" = game_server:send_recv(Socket, RegisterCom),
    ["name"] = game_server:send_recv(Socket, ListAccCom).

try_register_existing_account(Config)->
    RegisterCommand = ?config(register_command, Config),
    ListAccCom = ?config(list_accounts, Config),
    Socket = ?config(socket, Config),
    ["name"] = game_server:send_recv(Socket, ListAccCom),
    "error, already_registered" = game_server:send_recv(Socket, RegisterCommand).

deregister_existing_account(Config)->
    DeregisterCommand = ?config(deregister_command, Config),
    ListAccCom = ?config(list_accounts, Config),
    Socket = ?config(socket, Config),
    ["name"] = game_server:send_recv(Socket, ListAccCom),
    "ok, deregistered" = game_server:send_recv(Socket, DeregisterCommand),
    [] = game_server:send_recv(Socket, ListAccCom).

try_deregister_nonexisting_account(Config)->
    DeregisterCommand = ?config(deregister_command, Config),
    ListAccCom = ?config(list_accounts, Config),
    Socket = ?config(socket, Config),
    [] = game_server:send_recv(Socket, ListAccCom),
    "error, not_found" = game_server:send_recv(Socket, DeregisterCommand).
    
try_undefined_command(Config)->
    UndefinedCommand = ?config(undefined_command, Config),
    Socket = ?config(socket, Config),
    "error, no_command_found" = game_server:send_recv(Socket, UndefinedCommand).
    
