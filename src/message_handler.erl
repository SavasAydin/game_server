-module(message_handler).

-behaviour(gen_server).

-include("include/account.hrl").

-export([start_link/1,
	 start_handler/1,
	 stop/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(state, {}).

start_handler(Name) ->
    supervisor:start_child(msg_handler_sup, [Name]).

start_link(Name) ->
    Instance = atom_to_list(?MODULE) ++ "_" ++ Name,
    gen_server:start_link({local, list_to_atom(Instance)},?MODULE,[],[]).

stop() -> 
    gen_server:cast(?MODULE, stop).

init([]) ->
    process_flag(trap_exit, true),
    {ok, #state{}}.

handle_call(_Msg, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(stop, State) ->
    {stop, normal, State}.

handle_info({tcp, Socket, Data}, State) ->
    gen_tcp:send(Socket, Data),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

