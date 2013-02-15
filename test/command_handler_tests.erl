-module(command_handler_tests).

-include_lib("eunit/include/eunit.hrl").
-include("include/command.hrl").

handle_undefined_command_test()->
    UndefinedCommand = #server_command{type = undefined_command},
    ?assertEqual({error, undefined_command}, 
		 command_handler:perform(UndefinedCommand)).

handle_server_commands_test_() ->    
    {setup,
     fun setup/0,
     fun clean/1,
     [
      {"handle register server command",
       fun handle_register_command/0},
      {"handle reregister server command",
       fun handle_reregister_command/0}
     ]}.

   
handle_register_command() ->
    Name = "name",
    Password = "pass",
    meck:expect(mnesia, read, fun(_) -> [] end),
    meck:expect(mnesia, transaction, fun(_) -> {atomic, [Name, Password]} end),
    RegisterCommand = #server_command{type = register,
				      arguments = [Name, Password]},
    ?assertEqual({ok, registered}, command_handler:perform(RegisterCommand)).

handle_reregister_command() ->
    Name = "name",
    Password = "pass",
    meck:expect(mnesia, read, fun(_) ->
				      [Name, Password] end),
    RegisterCommand = #server_command{type = register,
				      arguments = [Name, Password]},
    ?assertEqual({error, already_registered}, command_handler:perform(RegisterCommand)).


setup() ->
    meck:new(mnesia).

clean(_) ->
    meck:unload(mnesia).
    
