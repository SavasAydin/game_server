-module(command_parser).

-export([parse/1]).

-include("include/command.hrl").
-include("include/account.hrl").

parse("") ->
    {error, empty_command};
parse(Command) ->
    {[Type], Args} = lists:split(1,tuple_to_list(Command)),
    #server_command{type = Type,
		    arguments = Args}.
