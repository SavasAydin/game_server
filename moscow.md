game_server
===========

A Generic game server developed using Erlang. Aims to host online games. 

MUST
M1: A tcp server accepts connections on specified port from one or more computers.
   
M2: User can register.
Registration gives you free member username and password 
That is essential to access the game_server.
Username is unique for each user.
-spec(register(string(),string()) -> {ok, registered} | 
				     {error, already_registered}).
Register command stores username and password in Mnesia database.    	     
M3: Client can deregister.

M4: A database is used to store user information

M5: Mochiweb

SHOULD
S1: Users can send IM 


