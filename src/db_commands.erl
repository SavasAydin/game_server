-module(db_commands).

-export([new/0,
	 install/0,
	 insert/2,
	 db_to_list/0
	]).

-record(account, {name, password}).

install() ->
    mnesia:create_schema([node() | nodes()]),
    application:start(mnesia).

new() ->
    Nodes = [node() | nodes()],
    mnesia:create_table(account, 
			[{attributes, record_info(fields, account)},
			 {ram_copies, Nodes}
			]),
    account.
    
db_to_list() ->
    F = fun() -> mnesia:match_object(#account{_ = '_'}) end,
    {atomic, Accounts} = mnesia:transaction(F),
    Accounts.


insert(Account, Db) ->
    F = fun() -> mnesia:write(Db, Account, write) end,
    {atomic, _} = mnesia:transaction(F),
    Db.

