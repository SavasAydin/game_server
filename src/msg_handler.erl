-module(msg_handler).

-behaviour(gen_server).

-include("include/account.hrl").

-export([start_link/0, 
	 register_account/1,
	 get_accounts/0,
	 deregister_account/1,
	 stop/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(state, {accounts}).

start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

register_account(Account) -> gen_server:call(?MODULE, {register, Account}).

get_accounts() -> gen_server:call(?MODULE, get_accounts).

deregister_account(Account) -> gen_server:call(?MODULE, {deregister, Account}).
    
stop() -> gen_server:cast(?MODULE, stop).

init([]) ->
    process_flag(trap_exit, true),
    db_commands:install(),
    db_commands:new(),
     ok = mnesia:wait_for_tables([account], 100),
    Accounts = db_commands:db_to_list(),
    {ok, #state{accounts = Accounts}}.

handle_call({register, Account}, _From, State) ->
    {reply, db_commands:insert(Account, account), State#state{accounts=db_commands:db_to_list()}};
handle_call(get_accounts, _From, State) ->
    {reply, State#state.accounts, State};
handle_call({deregister, Account}, _From, State) ->
    {reply, db_commands:delete(Account, account), State#state{accounts=db_commands:db_to_list()}};
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

