build: clean
	mkdir -p .build
	erlc -o .build/ *.erl

run: build
	cd .build && erl -noshell -eval 'opty:start(5,50,2,2,5)'

clean:
	rm -rf .build
	rm -f *.beam
