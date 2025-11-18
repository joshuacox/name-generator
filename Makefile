.PHONY: all test testx homepage github commit

all: name-generator name-generator_cpp ${HOME}/.cargo/target/debug/name-generator name-generator_go NameGenerator.class name_generator.beam name-generator.jar NameGeneratorScala SLOCCOUNT.md

clean:
	-@rm -v name-generator 
	-@rm -v name-generator_cpp
	-@rm -v ${HOME}/.cargo/target/debug/name-generator
	-@rm -v NameGenerator.class
	-@rm -v name-generator_go
	-@rm -v name_generator.beam
	-@rm -v NameGeneratorScala.class
	-@rm -v SLOCCOUNT.md

github:
	${BROWSER} https://github.com/joshuacox/name-generator/ &

homepage:
	${BROWSER}  https://joshuacox.github.io/name-generator/ &

name-generator:
	gcc -O3 name-generator.c -o name-generator

name-generator_cpp:
	g++ -O3 name-generator.cpp -o name-generator_cpp

${HOME}/.cargo/target/debug/name-generator:
	$(MAKE) -C rust all

name-generator_go:
	go build -O3 -o name-generator_go name-generator.go

NameGenerator.class:
	javac NameGenerator.java

test:
	./test/bats/bin/bats -x test/test.bats


name_generator.beam:
	erl -compile name_generator

BENCHMARK.md:
	./meta-benchmark.sh | tee BENCHMARK.md

benchmark.cast:
	time asciinema rec --command "./meta-benchmark.sh" benchmark.cast

name-generator.jar:
	kotlinc name-generator.kt -include-runtime -d name-generator.jar

# WIPs
#
name-generator_zig:
	zig build-exe -I . name-generator_zig.zig -lc

dmd name-generator_d:
	dmd name-generator_d.d

name-generator_pony:
	ponyc -b name-generator_pony

commit:
	aider --commit --model=ollama_chat/llama3.2

NameGeneratorScala:
	scalac NameGeneratorScala.scala

SLOCCOUNT.md:
	sloccount ./ > SLOCCOUNT.md
