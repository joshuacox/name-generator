.PHONY: all test testx

all: name-generator name-generator_cpp ${HOME}/.cargo/target/debug/name-generator name-generator_go NameGenerator.class name_generator.beam

clean:
	-@rm -v name-generator 
	-@rm -v name-generator_cpp
	-@rm -v ${HOME}/.cargo/target/debug/name-generator
	-@rm -v NameGenerator.class
	-@rm -v name-generator_go
	-@rm -v name_generator.beam

name-generator:
	gcc name-generator.c -o name-generator

name-generator_cpp:
	g++ name-generator.cpp -o name-generator_cpp

${HOME}/.cargo/target/debug/name-generator:
	$(MAKE) -C rust all

name-generator_go:
	go build -o name-generator_go name-generator.go

NameGenerator.class:
	javac NameGenerator.java

test:
	./test/bats/bin/bats test/test.bats

testx:
	./test/bats/bin/bats -x test/test.bats

name_generator.beam:
	erl -compile name_generator
