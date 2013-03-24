-module(db_commands).

-export([new/0,
	 install/0,
	 db_to_list/0,
	 insert/2,
	 get_account/2
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
    lists:reverse(Accounts).

insert(Account, Db) ->
    F = fun() -> mnesia:write(Db, Account, write) end,
    {atomic, _} = mnesia:transaction(F),
    Db.

get_account(AccountName, Db) ->
    F = fun() -> case mnesia:read(Db, AccountName) of 
		     [] ->
			 {error, not_exists};
		     Account ->
			 Account
		 end
	end,
    {atomic, Account} = mnesia:transaction(F),
    Account.
