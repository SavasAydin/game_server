-module(gameLib).

-export([
	 encode/1
	 ,decode/1
	]).

encode(Term)->
    term_to_binary(Term).

decode(Binary)->
    binary_to_term(Binary).
