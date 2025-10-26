build: clean
	mkdir -p .build
	erlc -o .build/ *.erl

run: build
	cd .build && erl -noshell -eval 'timey:start(5,50,10,5)'

clean:
	rm -rf .build
	rm -f *.beam
