game_server
===========

A Generic game server developed using Erlang. Aims to host online games. 

MUST
M1: A game server is a tcp server that accepts connections on specified port from one or more computers.
* It receives requests over a TCP socket (Joe has a nice robust tcp server implementation).

M2: Client can register to the game server
* Asks server to register; {name, password} -> {ok, registered} | {error, is_already_exist}
* Name must be unique.

M3: Client can login to the game server to place in a game.

M4: A communication module is needed between game server and clients to handle requests.
* It trasmits clients requests; registration and unregistration, logging in and out, or game wished to play

M5: Clients can log out.
* Asks server to log out 

M6: Client can unregister
* Asks server to unregister.

M7: Clients can login to be placed for the game
* An organizer will place users into session

M8: Game server should run different online games (?)

M9: A database can store state of users
* Mnesia 

SHOULD
S1: Mochiweb (?)

COULD
C1: Users can send IM 
* {Name, Msg} -> {ok, sent} | {error, no_such_user}


WOULD
W1: All the users could chat in one chatbox


Sprint Plan
* Planing and started M1, M2
* M1,M2,M4,M5,M6,M9
* M3, M7, S1
* M8, C1, W1

