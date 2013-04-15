-module(tcp_server_sup).
-behaviour(supervisor).

-export([start_link/0, stop/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

stop() ->
    exit(whereis(?MODULE), shutdown).

init([]) ->
    RestartStrategy = {rest_for_one, 5, 2000},
    TcpServer = child(tcp_server, worker, {message_handler, start_handler}),
    MsgHandlerSup = child(msg_handler_sup, supervisor),
    Children = [TcpServer, MsgHandlerSup],
    {ok, {RestartStrategy, Children}}.

child(Module, Type) ->
    {Module, {Module, start_link, []},
     permanent, brutal_kill, Type, [Module]}.
child(Module, Type, Args) ->
    {Module, {Module, start_link, [Args]},
     permanent, brutal_kill, Type, [Module]}.


