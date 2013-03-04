-module(game_server_sup).
-behaviour(supervisor).

-export([start_link/0,
	 start_socket/0,
	 init/1
	]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    {ok, {{one_for_one, 60, 3600},
	  [{game_server,
	    {game_server, start_link, []},
	    temporary, 1000, worker, [game_server]}
	  ]}}.

start_socket() ->
    supervisor:start_child(?MODULE, []).
 
