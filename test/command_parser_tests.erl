-module(command_parser_tests).

-include_lib("eunit/include/eunit.hrl").

-include("include/command.hrl").

parse_empty_command_test() ->
    Command = "",
    ?assertEqual({error, empty_command}, command_parser:parse(Command)).

parse_server_commands_test_() ->
    [
     {"parse register server command",
     fun parse_register_command/0},
     {"parse deregister server command",
     fun parse_deregister_command/0},
     {"parse list accounts server command",
     fun parse_list_accounts_command/0}
    ].

parse_register_command() ->
    Name = "name",
    Password = "password",
    Command = {register, Name, Password},
    ?assertEqual(#server_command{type = register,
				 arguments = [Name, Password]},
		 command_parser:parse(Command)).

parse_deregister_command() ->
    Name = "name",
    Password = "password",
    Command = {deregister, Name, Password},
    ?assertEqual(#server_command{type = deregister,
				 arguments = [Name, Password]},
		 command_parser:parse(Command)).

parse_list_accounts_command() ->
    Command = {list_accounts},
    ?assertEqual(#server_command{type = list_accounts},
		 command_parser:parse(Command)).

