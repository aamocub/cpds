build: clean
	mkdir -p .build
	erlc -o .build/ *.erl

clean:
	rm -rf build
	rm -f *.beam