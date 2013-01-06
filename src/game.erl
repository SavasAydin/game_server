-module(game).

-export([
	 handle_command/1
	]).

-record(account,{name, password}).
		   
handle_command({"register", Name, Pass}) ->
    store_account(Name, Pass);
handle_command({"deregister", Name}) ->
    delete_account(Name);
handle_command({"list_accounts"}) ->
    list_accounts();
handle_command(_Other) ->
    "error, no_command_found".

-spec(list_accounts()->[#account{}]).
list_accounts()->
    F = fun()->
		MatchHead = #account{name='$1', _='_'},
		Guard = [],
		Result = ['$1'],
		mnesia:select(account,[{MatchHead, Guard, Result}])
	  end,
    mnesia:activity(transaction, F).

-spec(store_account(#account{}, #account{}) -> {ok, term()} | {error, term()}).
store_account(Name, Pass)->
    F = fun()->
		case find_account(Name) of
		    not_found->
			mnesia:write(#account{name=Name
					      ,password=Pass}),
			"ok, registered";
		    found ->
			"error, already_registered"
		end
	end,
    mnesia:activity(transaction, F).

-spec(delete_account(#account{})-> {ok, term()} | {error, term()}).
delete_account(Name)->
    F = fun()->
		case find_account(Name) of
		    not_found ->
			"error, not_found";
		    found->
			mnesia:delete({account, Name}),
			"ok, deregistered"
		end
	end,
    mnesia:activity(transaction, F).

-spec(find_account(#account{}) -> term()).
find_account(Name)->
    case mnesia:read({account, Name}) of 
	[]->
	    not_found;
	_Any ->
	    found
    end.
		    
