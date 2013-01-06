-module(game_server_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([
	 all/0
	 ,init_per_suite/1 
	 ,end_per_suite/1
%% 	 ,init_per_testcase/2 
%% 	 ,end_per_testcase/2
	]).

-export([
	 register_test_case/1
	]).

-define(TCP_OPTIONS, [binary, {reuseaddr, true},{packet, 0}, {active, false}]).
-define(PORT, 8080).


all()->
    [register_test_case].

init_per_suite(Config)->
    Priv = ?config(priv_dir, Config),
    application:set_env(mnesia, dir, Priv),
    db_server:init(),
    ok = game_server:start(),
    Config.
end_per_suite(_Config)->
    ok.

register_test_case(Config)->
    Socket = ?config(socket, Config),
    Name = "name",
    Pass = "pass",
    RegisterCommand = {"register", Name, Pass}, 
    DecodedCommand = gameLib:encode(RegisterCommand),
    {ok,Socket} = gen_tcp:connect("localhost",?PORT,?TCP_OPTIONS),
    ok = gen_tcp:send(Socket, DecodedCommand),
    {ok, Reply} = gen_tcp:recv(Socket, 0),
    "error, already_registered" = gameLib:decode(Reply).

