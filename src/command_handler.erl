-module(command_handler).

-export([perform/1,
	 install/0
	]).

-include("include/command.hrl").
-include("include/account.hrl").

perform(Command = #server_command{type=register}) ->
    register(Command#server_command.arguments);
perform(_) ->
    {error, undefined_command}.

register([Name, Pass]) ->
    case mnesia:read({account, Name}) of
	[]->
	    write_to_mnesia(Name, Pass),
	    {ok, registered};
	_ ->
	    {error, already_registered}
    end.

install() ->
    Nodes = [node()],
    ok = mnesia:create_schema(Nodes),
    rpc:multicall(Nodes, application, start, [mnesia]),
    AccountTable = create_table(Nodes),
    rpc:multicall(Nodes, application, stop, [mnesia]),
    case AccountTable of 
	{atomic, ok} ->
	    ok;
	{aborted, already_exist}  ->
	    ok;
	{aborted, Else} ->
	    {error, Else}
    end.

create_table(Nodes) ->
    mnesia:create_table(account, 
			[{disc_copies,Nodes}
			 ,{attributes,record_info(fields, account)}
			 ,{type, bag}
			]).

write_to_mnesia(Name, Pass) ->
    F = fun() ->
		mnesia:write(#account{name=Name,
				      password=Pass})
	end,
    {atomic, ok} = mnesia:transaction(F).			
	
