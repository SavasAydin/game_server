-module(game_server_sup).
-behaviour(supervisor).

-export([start_link/0,
	 start_socket/0,
	 init/1
	]).

-define(PORT, 8080).
-define(TCP_OPTIONS, [{active, once}, {packet, 0}]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    {ok, ListenSocket} = gen_tcp:listen(?PORT, ?TCP_OPTIONS),
    spawn_link(fun available_listeners/0),
    {ok, {{simple_one_for_one, 60, 3600},
	  [{socket,
	    {game_server, start_link, [ListenSocket]},
	    temporary, 1000, worker, [game_server]}
	  ]}}.

start_socket() ->
    supervisor:start_child(?MODULE, []).
 
available_listeners() ->
    [start_socket() || _ <- lists:seq(1,20)],
    ok.
