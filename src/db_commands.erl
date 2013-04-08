-module(db_commands).

-export([new/0,
	 create_schema_if_not_exist/0,
	 delete_schema_if_exists/0,
	 db_to_list/0,
	 insert/2,
	 delete/2,
	 get_account/2
	]).

-include("include/account.hrl").
    
create_schema_if_not_exist() ->
    case mnesia:system_info(db_nodes) of 
	[] ->
	    mnesia:create_schema([node() | nodes()]);
	[_] ->
	    ok
    end.

delete_schema_if_exists() ->
    case mnesia:system_info(db_nodes) of 
	[] ->
	    ok;
	Nodes ->
	    mnesia:delete_schema(Nodes)
    end.

new() ->
    Nodes = [node() | nodes()],
    mnesia:create_table(account, 
			[{attributes, record_info(fields, account)},
			 {ram_copies, Nodes}
			]),
    ok = mnesia:wait_for_tables([account], 100),
    account.
    
db_to_list() ->
    F = fun() -> mnesia:match_object(#account{_ = '_'}) end,
    {atomic, Accounts} = mnesia:transaction(F),
    lists:reverse(Accounts).

insert(Account, Db) ->
    F = fun() -> insert_unless_account_exists(Account, Db) end,
    {atomic, Reply} = mnesia:transaction(F),
    Reply.

insert_unless_account_exists(Account, Db) ->
    case mnesia:read(Db, Account#account.name) of 
	[] ->
	    mnesia:write(Db, Account, write);
	[_] ->
	    {error, already_exist}
    end.

get_account(AccountName, Db) ->
    F = fun() -> get_if_account_exists(AccountName, Db) end, 
    {atomic, Account} = mnesia:transaction(F),
    Account.

get_if_account_exists(AccountName, Db) ->
    case mnesia:read(Db, AccountName) of 
	[] ->
	    {error, not_exist};
	Account ->
	    Account
    end.

delete(Account, Db) ->
    F = fun() -> delete_if_account_exists(Account, Db) end,
    {atomic, Reply} = mnesia:transaction(F),
    Reply.

delete_if_account_exists(Account, Db) ->
    case mnesia:read(Db, Account#account.name) of 
	[] ->
	    {error, not_exist};
	[Account] ->
	    mnesia:delete(Db, Account#account.name, write)	    
    end.

		
