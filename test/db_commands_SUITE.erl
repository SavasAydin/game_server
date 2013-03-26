-module(db_commands_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([all/0,
	 init_per_suite/1,
	 init_per_testcase/2,
	 end_per_testcase/2,
	 end_per_suite/1
	]).
-export([insert_account/1,
	 insert_account_with_same_name/1,
	 get_account_by_name/1,
	 delete_existing_account/1,
	 delete_non_existing_account/1
	]).

-record(account, {name, password}).

all() ->
    [insert_account,
     insert_account_with_same_name,
     get_account_by_name,
     delete_existing_account,
     delete_non_existing_account
    ].

init_per_suite(Config) ->
    db_commands:install(),
    Config.

end_per_suite(_Config) ->
    mnesia:stop().

init_per_testcase(_TestCase, Config) ->
    Db = db_commands:new(),
    Savas = create_account(savas, pass),
    [{db, Db}, {savas, Savas} | Config].

end_per_testcase(_TestCase, Config) ->
    Db = ?config(db, Config),
    mnesia:delete_table(Db),
    Config.

insert_account(Config) ->
    Db = ?config(db, Config),
    Savas = ?config(savas, Config),
    [] = db_commands:db_to_list(),
    ok =db_commands:insert(Savas, Db),
    [Savas] = db_commands:db_to_list().

insert_account_with_same_name(Config) ->
    Db = ?config(db, Config),
    Savas = ?config(savas, Config),
    SavasDifferentPassword = create_account(savas, different_pass),
    ok =db_commands:insert(Savas, Db),
    {error, already_exist} = db_commands:insert(SavasDifferentPassword, Db),
    [Savas] = db_commands:db_to_list().
    
get_account_by_name(Config) ->
    Db = ?config(db, Config),
    Savas = ?config(savas, Config),
    Gianfranco = create_account(gianfranco, pass_alongi),
    ok = db_commands:insert(Savas, Db),
    ok = db_commands:insert(Gianfranco, Db),
    [Savas,Gianfranco] = db_commands:db_to_list(),
    [Gianfranco] = db_commands:get_account(gianfranco, Db),
    {error, not_exist} = db_commands:get_account(simon, Db). 

delete_existing_account(Config) ->
    Db = ?config(db, Config),
    Savas = ?config(savas, Config),
    ok = db_commands:insert(Savas, Db),
    [Savas] = db_commands:db_to_list(),
    ok = db_commands:delete(Savas, Db),
    [] = db_commands:db_to_list().

delete_non_existing_account(Config) ->
    Db = ?config(db, Config),
    Savas = ?config(savas, Config),
    [] = db_commands:db_to_list(),
    {error, not_exist} = db_commands:delete(Savas, Db).
    
create_account(Name, Password) ->
    #account{name = Name, password = Password}.

