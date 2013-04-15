-module(tcp_server).

-export([start_link/1, stop/0]).
-export([accepts_then_listens_for_upcoming_connection/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-include("include/account.hrl").

-record(state, {loop :: {string(), string()},
		lsocket 
	       }).

start_link(Loop) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Loop, []).

stop() -> 
    gen_server:cast(?MODULE, stop).

init(Loop) ->
    process_flag(trap_exit, true),
    {ok, LSocket} = gen_tcp:listen(?PORT, ?TCP_OPTIONS),
    State = #state{lsocket = LSocket, loop = Loop},
    {ok, start_accepting_process(State)}.

start_accepting_process(State) ->
    process_flag(trap_exit, true),
    proc_lib:spawn_link(?MODULE, 
			accepts_then_listens_for_upcoming_connection,
			[{self(), State#state.lsocket, State#state.loop}]),
    State.

accepts_then_listens_for_upcoming_connection({Server, LSocket, {M, F}}) ->
    {ok, Socket} = gen_tcp:accept(LSocket),
    gen_server:cast(Server, accepted),
    UniqueExtension = binary_to_list(term_to_binary(Socket)),
    M:F(UniqueExtension).

handle_cast(accepted, State) ->
    {noreply, start_accepting_process(State)};
handle_cast(stop, State) ->
    {stop, normal, State}.

handle_call(_Msg, _Caller, State) ->
     {reply, ok, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

