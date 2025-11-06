.PHONY: all test testx envtest

all: name-generator ${HOME}/.cargo/target/debug/name-generator name-generator_go NameGenerator.class

name-generator:
	gcc name-generator.c -o name-generator

${HOME}/.cargo/target/debug/name-generator:
	$(MAKE) -C rust all

clean:
	rm -v name-generator 
	rm -v ${HOME}/.cargo/target/debug/name-generator

name-generator_go:
	go build -o name-generator_go name-generator.go

NameGenerator.class:
	javac NameGenerator.java

test:
	./test/bats/bin/bats test/test.bats

testx:
	./test/bats/bin/bats -x test/test.bats

envtest:
	./test/bats/bin/bats -x test/envtest.bats
