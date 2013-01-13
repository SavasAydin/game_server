struct:
	mkdir -p src/
	mkdir -p test/
	mkdir -p ebin/
	mkdir -p inc/
all:
	erlc -o ebin/ src/*.erl test/*.erl

clean:
	rm -rf *~
	rm -rf test/*.*~
	rm -rf test/*.beam
	rm -rf src/*.*~
	rm -rf ebin/*.beam
	rm -rf logs/*
	rm -rf rel/*.*~
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


