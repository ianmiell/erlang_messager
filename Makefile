ERL=erl
NODE_NAME=message_server

all:
	$(ERL) -make

doc:	
	$(ERL) -pa `pwd`/ebin \
	-noshell 
	
clean:
	rm -fv ebin/*.beam
	rm -fv erl_crash.dump

clean-doc:
	rm -fv doc/*.html
	rm -fv doc/edoc-info
	rm -fv doc/*.css

