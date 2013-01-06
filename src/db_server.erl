-module(db_server).

-export([
	 start/2
	 ,stop/1
	 ,init/0
	]).

-record(account,{name, password}).
    
init() ->
    Nodes = [node()],
    ok = mnesia:create_schema(Nodes),
    rpc:multicall(Nodes, application, start, [mnesia]),
    mnesia:create_table(account, 
			[
			 {disc_copies,Nodes}
			 ,{attributes,record_info(fields, account)}
			 ,{type, bag}
			]),
    rpc:multicall(Nodes, application, stop, [mnesia]).

start(normal, []) ->
    mnesia:wait_for_tables([account], 5000),
    db_server_sup:start_link().
 
stop(_) ->
     ok.

		    
