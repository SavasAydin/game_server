-module(db_commands_SUITE).

-include_lib("common_test/include/ct.hrl").
-include("account.hrl").

-export([all/0,
	 init_per_suite/1,
	 init_per_group/2,
	 end_per_group/2,
	 end_per_suite/1,
	 groups/0
	]).
-export([insert_account/1,
	 insert_account_with_same_name/1,
	 get_account_by_name_if_exists/1,
	 get_account_returns_error_if_not_exist/1,
	 delete_existing_account/1,
	 delete_non_existing_account/1
	]).

all() ->
    [{group, insert},
     {group, delete}
    ].

groups() ->
    [{insert, [], insert_related_cases()},
     {delete, [], delete_related_cases()}
    ].

insert_related_cases() ->
    [insert_account,
     get_account_by_name_if_exists,
     insert_account_with_same_name
     ].

delete_related_cases() ->
    [delete_existing_account,
     get_account_returns_error_if_not_exist,
     delete_non_existing_account
    ].

init_per_suite(Config) ->
    ok = db_commands:create_schema_if_not_exist(),
    ok = mnesia:start(),
    Config.

end_per_suite(_Config) ->
    stopped = mnesia:stop(),
    ok = db_commands:delete_schema_if_exists().
    
init_per_group(insert, Config) ->
    Db = db_commands:new(),
    [{db, Db} | Config];
init_per_group(delete, Config) ->
    Db = db_commands:new(),
    Savas = create_account(savas, pass),
    ok =db_commands:insert(Savas, Db),
    [{db, Db}, {savas, Savas} | Config].

end_per_group(insert, Config) ->
    Db = ?config(db, Config),
    mnesia:delete_table(Db),
    Config;
end_per_group(delete, Config) ->
    Db = ?config(db, Config),
    mnesia:delete_table(Db),
    Config.

insert_account(Config) ->
    Db = ?config(db, Config),
    Savas = create_account(savas, pass),
    [] = db_commands:db_to_list(),
    ok =db_commands:insert(Savas, Db),
    [Savas] = db_commands:db_to_list().

get_account_by_name_if_exists(Config) ->
    Db = ?config(db, Config),
    [Savas] = db_commands:db_to_list(),
    [Savas] = db_commands:get_account(savas, Db).

insert_account_with_same_name(Config) ->
    Db = ?config(db, Config),
    SavasDifferentPassword = create_account(savas, different_pass),
    [Savas] = db_commands:db_to_list(),
    {error, already_exist} = db_commands:insert(SavasDifferentPassword, Db),
    [Savas] = db_commands:db_to_list().

delete_existing_account(Config) ->
    Db = ?config(db, Config),
    [Savas] = db_commands:db_to_list(),
    ok = db_commands:delete(Savas, Db),
    [] = db_commands:db_to_list().
    
get_account_returns_error_if_not_exist(Config) ->
    Db = ?config(db, Config),
    [] = db_commands:db_to_list(),
    {error, not_exist} = db_commands:get_account(savas, Db). 

delete_non_existing_account(Config) ->
    Db = ?config(db, Config),
    Savas = ?config(savas, Config),
    [] = db_commands:db_to_list(),
    {error, not_exist} = db_commands:delete(Savas, Db).
    
create_account(Name, Password) ->
    #account{name = Name, password = Password}.
