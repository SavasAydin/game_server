-module(db_commands_tests).

-include_lib("eunit/include/eunit.hrl").

new_table_test() ->
    ?assertEqual({atomic, ok}, db_commands:empty()).

