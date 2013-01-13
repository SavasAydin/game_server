Rebar=./rebar

.PHONY: 

compile:
	$(Rebar) compile

eunit:
	$(Rebar) eunit

ct:
	$(Rebar) ct skip-deps=true

release:
	cd rel/
	$(Rebar) compile generate

##rebar release
start:
	rel/game_server/bin/game_server start

attach:
	rel/game_server/bin/game_server attach

stop:
	rel/game_server/bin/game_server stop	

##
all:
	erlc -o ebin/ src/*.erl test/*.erl

struct:
	mkdir -p src/
	mkdir -p test/
	mkdir -p ebin/
	mkdir -p inc/

clean:
	$(Rebar) clean
	rm -rf *~
	rm -rf test/*.*~
	rm -rf test/*.beam
	rm -rf src/*.*~
	rm -rf rel/*.*~
	rm -fr test/logs/*
	rm -rf Mnesia.nonode@nohost	

init:
	erl -pa ebin/-eval 'db_server:init().' -s init stop

test:
	erl -pa ebin/ -eval 'eunit:test({dir, "ebin/"}, [verbose]).' -s init stop

pack:
	make clean
	cd ..; tar hcvf - game_server | gzip > game_server.tgz

ct:
	ct_run -pa ebin/ -spec game_server.spec


