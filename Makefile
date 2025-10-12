build: clean
	mkdir -p .build
	erlc -o .build/ *.erl

run: build
	cd .build && erl -eval 'paxy:start([0, 0, 0, 0, 0])'

ft: build
	cd .build && erl -eval 'paxy:start([10000, 10000, 10000, 10000, 9000])'

remote: build
	cd .build ; \
	erl -sname paxy-acc@localhost -setcookie paxy -noshell </dev/null & \
	erl -sname paxy-pro@localhost -setcookie paxy -noshell </dev/null & \
	erl -sname     test@localhost -setcookie paxy -eval 'remote_paxy:start([0, 0, 0])'

clean:
	rm -rf .build
	rm -f *.beam
