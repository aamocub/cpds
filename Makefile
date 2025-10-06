build: clean
	mkdir -p .build
	erlc -o .build/ *.erl

exp4: build
	cd .build && erl -eval 'paxy:start([2000, 2000, 2000, 2000, 2000])'

clean:
	rm -rf build
	rm -f *.beam