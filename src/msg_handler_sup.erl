-module(msg_handler_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
    io:format("start msg_handler_sup ..~n"),
    supervisor:start_link({local, ?MODULE}, ?MODULE, no_args).

init(no_args) ->
    io:format("init msg_handler_sup ...~p~n", [?MODULE]),
    {ok, {{one_for_one, 5, 2000},
          [child(msg_handler)]}}.

child(Module) ->
    {Module, {Module, start_link, []}, 
     permanent, brutal_kill, worker, [Module]}.


