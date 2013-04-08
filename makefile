all:
	erlc -o ebin/ -I src/ src/*.erl test/*.erl

dialyzer:
	dialyzer src/*.erl -I src/

eunit:
	erl -pa ebin/ -eval 'eunit:test(msg_handler,[verbose]),\
			     eunit:test(tcp_server,[verbose]),\
			     init:stop().'	

clean:
	rm ebin/*.beam