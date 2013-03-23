%%%-------------------------------------------------------------------
%%% @author Savas Aydin <savasaydin@gmail.com>
%%% @copyright (C) 2013, Savas Aydin
%%% @doc
%%%
%%% @end
%%% Created : 23 Mar 2013 by Savas Aydin <savasaydin@gmail.com>
%%%-------------------------------------------------------------------
-module(msg_handler).

-behaviour(gen_server).

-export([start_link/0, register/1, get_accounts/0, stop/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(account, {name, password}).
-record(state, {accounts}).

start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

register(Account) -> gen_server:call(?MODULE, {register, Account}).

get_accounts() -> gen_server:call(?MODULE, get_accounts).

stop() -> gen_server:cast(?MODULE, stop).

init([]) ->
    process_flag(trap_exit, true),
    {ok, #state{accounts = []}}.

handle_call({register, Account}, _From, State) ->
    {reply, registered, State#state{accounts = Account}};
handle_call(get_accounts, _From, State) ->
    {reply, State#state.accounts, State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

create_account(Name, Password) ->
    #account{name = Name, password = Password}.
