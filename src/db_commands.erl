-module(db_commands).

-export([empty/0,
	install/0
	]).

-record(account, {name, password}).

install() ->
    Nodes = [node() | nodes()],
    mnesia:create_schema(Nodes),
    application:start(mnesia).

empty() ->
    Nodes = [node() | nodes()],
    mnesia:create_table(account, 
			[{attributes, record_info(fields, account)},
			 {ram_copies, Nodes}
			]).
    
