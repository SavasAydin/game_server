-record(account, {name,
		  password}).

-define(PORT, 8080).

-define(TCP_OPTIONS, [binary, {packet, 0}, {active, once}]).
	
