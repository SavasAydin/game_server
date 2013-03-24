all:
	erlc -o ebin/ -I src/ src/*.erl test/*.erl

init:
	erl -pa ebin/-eval 'db_server:init().' -s init stop

.PHONY: eunit
eunit:
        erl -pa ebin/ -eval 'eunit:test(msg_handler,[verbose]),\
                             init:stop().'

clean:
	rm ebin/*.beam
