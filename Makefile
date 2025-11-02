all: name-generator ${HOME}/.cargo/target/debug/name-generator

name-generator:
	gcc name-generator.c -o name-generator

${HOME}/.cargo/target/debug/name-generator:
	$(MAKE) -C rust all

clean:
	rm -v name-generator 
	rm -v ${HOME}/.cargo/target/debug/name-generator
