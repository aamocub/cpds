build: clean
	mkdir -p .build
	erlc -o .build/ *.erl

run: build
	cd .build && erl -noshell -eval 'opty:start(5,50,2,2,5)'

remote: build
	cd .build ; \
	erl -sname opty-srv@localhost -noshell </dev/null & \
	erl -sname opty-test@localhost -eval 'remote_opty:start(5,50,2,2,5)'

clean:
	rm -rf .build
	rm -f *.beam
