-record(server_command, {type :: register | deregister | list_accounts,
			 arguments = [] :: [string()] 
			}). 

-record(game_command, {type :: join | call | fold | raise | leave
		      }).
