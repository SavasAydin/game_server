-module(game_server_app).
-behaviour(application).
-export([start/2, stop/1]).

start(normal, []) ->
    game_server_sup:start_link().

stop(_) -> ok.

