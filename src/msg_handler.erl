-module(msg_handler).

-behaviour(gen_server).

-include("include/account.hrl").

-export([start_link/0, 
	 register_account/1,
	 get_accounts/0,
	 get_account/1,
	 deregister_account/1,
	 stop/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(state, {accounts}).

start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

register_account(Account) -> gen_server:call(?MODULE, {register, Account}).

get_accounts() -> gen_server:call(?MODULE, get_accounts).

get_account(AccountName) -> gen_server:call(?MODULE, {get_account, AccountName}).

deregister_account(Account) -> gen_server:call(?MODULE, {deregister, Account}).

stop() -> gen_server:cast(?MODULE, stop).

init([]) ->
    process_flag(trap_exit, true),
    ok = db_commands:create_schema_if_not_exist(),
    account = db_commands:new(),
    Accounts = db_commands:db_to_list(),
    {ok, #state{accounts = Accounts}}.

handle_call({register, Account}, _From, State) ->
    Reply = db_commands:insert(Account, account),
    Accounts = db_commands:db_to_list(),
    {reply, Reply, State#state{accounts = Accounts}};
handle_call(get_accounts, _From, State) ->
    {reply, State#state.accounts, State};
handle_call({get_account, AccountName}, _From, State) ->
    {reply, db_commands:get_account(AccountName, account), State};
handle_call({deregister, Account}, _From, State) ->
    Reply = db_commands:delete(Account, account),
    Accounts = db_commands:db_to_list(),
    {reply, Reply, State#state{accounts = Accounts}}.

handle_cast(stop, State) ->
    ok = db_commands:delete_schema_if_exists(),
    {stop, normal, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

