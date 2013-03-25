-module(db_commands).

-export([new/0,
	 install/0,
	 db_to_list/0,
	 insert/2,
	 get_account/2
	]).

-include("include/account.hrl").

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
    F = fun() -> insert_unless_account_exists(Account, Db) end,
    {atomic, Reply} = mnesia:transaction(F),
    Reply.

insert_unless_account_exists(Account, Db) ->
    case mnesia:read(Db, Account#account.name) of 
	[] ->
	    mnesia:write(Db, Account, write);
	_ ->
	    {error, already_exist}
    end.

get_account(AccountName, Db) ->
    F = fun() -> check_if_account_exists(AccountName, Db) end, 
    {atomic, Account} = mnesia:transaction(F),
    Account.

check_if_account_exists(AccountName, Db) ->
    case mnesia:read(Db, AccountName) of 
	[] ->
	    {error, not_exist};
	Account ->
	    Account
    end.
