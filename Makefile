clean:
	rm -rf build
	rm -f *.beam

build: clean
	mkdir -p .build
	erlc -o .build/ *.erl