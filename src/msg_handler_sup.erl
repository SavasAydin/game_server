-module(msg_handler_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    MsgHandler = child(message_handler),
    {ok, {{simple_one_for_one, 5, 2000},[MsgHandler]}}.

child(Module) ->
    {Module, {Module, start_link, []}, 
     permanent, brutal_kill, worker, [Module]}.


