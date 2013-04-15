-module(tcp_server).

-export([start_link/1, stop/0]).
-export([accept_loop/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-include("include/account.hrl").

-record(state, {loop,
		lsocket}).

start_link(Loop) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Loop, []).

init(Loop) ->
    process_flag(trap_exit, true),
    {ok, LSocket} = gen_tcp:listen(?PORT, ?TCP_OPTIONS),
    State = #state{lsocket = LSocket, loop = Loop},
    {ok, accept(State)}.

accept_loop({Server, LSocket, {M, F}}) ->
    {ok, Socket} = gen_tcp:accept(LSocket),
    gen_server:cast(Server, {accepted, self()}),
    Name = binary_to_list(term_to_binary(Socket)),
    M:F(Name).

accept(State = #state{lsocket=LSocket, loop = Loop}) ->
    process_flag(trap_exit, true),
    proc_lib:spawn_link(?MODULE, accept_loop, [{self(), LSocket, Loop}]),
    State.

stop() -> 
    gen_server:cast(?MODULE, stop).

handle_cast({accepted, _Pid}, State=#state{}) ->
    {noreply, accept(State)};
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

