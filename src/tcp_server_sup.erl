-module(tcp_server_sup).
-behaviour(supervisor).

-export([start_link/0, start_child/1, stop/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, no_args).

stop() ->
    exit(whereis(?MODULE), shutdown).

start_child(_) ->
    supervisor:start_child(?MODULE, [child(msg_handler_sup)]).

init(no_args) ->
     RestartStrategy = {rest_for_one, 5, 2000},
    {ok, {RestartStrategy, [child(tcp_server)]}}.

child(Module) ->
    {Module, {Module, start_link, []},
     permanent, brutal_kill, worker, [Module]}.
