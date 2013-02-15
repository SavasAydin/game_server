-module(game_server_SUITE).

-include_lib("common_test/include/ct.hrl").
-include("include/command.hrl").

-export([all/0,
	 group/0,
	 init_per_suite/1,
	 init_per_group/2,
	 end_per_group/2,
	 end_per_suite/1
	]).
-export([register_nonexisting_account/1,
	 try_register_existing_account/1,
	 try_undefined_command/1
	]).

-define(TCP_OPTIONS, [binary, {reuseaddr, true},{packet, 0}, {active, false}]).
-define(PORT, 8080).

all() ->
    [group, server_commands,
     try_undefined_command].

group() ->
    [
     {server_commands, 
      [],
      [register_nonexisting_account,
       try_register_existing_account
      ]
     }].
      
init_per_suite(Config) ->
    command_handler:install(),
    application:load(mnesia),
    application:load(game_server),
    application:start(mnesia),
    application:start(game_server),

    Name = "name",
    Pass = "pass",
    Register = #server_command{type=register,
			       arguments=[Name,Pass]},
    Undefined = #server_command{type=undefined},

    [{register_command, Register},
     {undefined_command, Undefined} | Config].

end_per_suite(_Config) ->
    application:stop(mnesia),
    application:stop(game_server).

init_per_group(server_commands, Config) ->
    {ok,Socket} = gen_tcp:connect("localhost",?PORT,?TCP_OPTIONS),
    [{socket, Socket} | Config].

end_per_group(server_commands, Config) ->
    Socket = ?config(socket, Config),
    gen_tcp:close(Socket).

register_nonexisting_account(Config) ->
    Register = ?config(register_command, Config),
    Socket = ?config(socket, Config),
    {ok, registered} = game_server:send_recv(Socket, Register).

try_register_existing_account(Config) ->
    Register = ?config(register_command, Config),
    Socket = ?config(socket, Config),
    {error, already_registered} = game_server:send_recv(Socket, Register).
    
try_undefined_command(Config) ->
    Undefined = ?config(undefined_command, Config),
    Socket = ?config(socket, Config),
    {error, no_command_found} = game_server:send_recv(Socket, Undefined).
