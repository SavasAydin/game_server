-module(db_commands_SUITE).
-include_lib("common_test/include/ct.hrl").

-export([all/0,
	 init_per_suite/1,
	 init_per_testcase/2,
	 end_per_testcase/2,
	 end_per_suite/1
	]).
-export([insert_account/1,
	 insert_multiple_and_get_by_name/1
	]).

-record(account, {name, password}).

all() ->
    [insert_account,
    insert_multiple_and_get_by_name].

init_per_suite(Config) ->
    db_commands:install(),
    Config.

end_per_suite(Config) ->
    mnesia:stop().

init_per_testcase(_TestCase, Config) ->
    Db = db_commands:new(),
    [{db, Db} | Config].

end_per_testcase(_TestCase, Config) ->
    Db = ?config(db, Config),
    mnesia:delete_table(Db),
    Config.

insert_account(Config) ->
    Db = ?config(db, Config),
    Savas = create_account(savas, pass),
    [] = db_commands:db_to_list(),
    db_commands:insert(Savas, Db),
    [Savas] = db_commands:db_to_list().

insert_multiple_and_get_by_name(Config) ->
    Db = ?config(db, Config),
    Savas = create_account(savas, pass_aydin),
    Gianfranco = create_account(gianfranco, pass_alongi),
    InsertSavas = db_commands:insert(Savas, Db),
    InsertGianfranco = db_commands:insert(Gianfranco, InsertSavas),
    [Savas,Gianfranco] = db_commands:db_to_list(),
    [Gianfranco] = db_commands:get_account(gianfranco, InsertGianfranco),
    {error, not_exists} = db_commands:get_account(simon, InsertGianfranco). 

create_account(Name, Password) ->
    #account{name = Name, password = Password}.

