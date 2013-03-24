-module(db_commands_SUITE).
-include_lib("common_test/include/ct.hrl").

-export([all/0,
	 init_per_suite/1,
	 init_per_testcase/2,
	 end_per_testcase/2,
	 end_per_suite/1
	]).
-export([insert_account/1]).

-record(account, {name, password}).

all() ->
    [insert_account].

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
    Db = db_commands:insert(Savas, Db),
    [Savas] = db_commands:db_to_list().

create_account(Name, Password) ->
    #account{name = Name, password = Password}.


