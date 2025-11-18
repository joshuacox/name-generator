.PHONY: all test testx homepage github commit rust data

all: name-generator name-generator_cpp name-generator_go NameGenerator.class name_generator.beam name-generator.jar rust/target/debug/name-generator name-generator_O2 name-generator_cpp_O2 name-generator_O1 name-generator_cpp_O1 NameGeneratorScala.class

clean:
	-@rm -v name-generator 
	-@rm -v name-generator_cpp
	-@rm -v rust/target/debug/name-generator
	-@rm -v NameGenerator.class
	-@rm -v name-generator_go
	-@rm -v name_generator.beam
	-@rm -v NameGeneratorScala.class

github:
	${BROWSER} https://github.com/joshuacox/name-generator/ &

homepage:
	${BROWSER}  https://joshuacox.github.io/name-generator/ &

name-generator:
	gcc -O3 name-generator.c -o name-generator

name-generator_cpp:
	g++ -O3 name-generator.cpp -o name-generator_cpp

name-generator_O2:
	gcc -O2 name-generator.c -o name-generator_O2

name-generator_cpp_O2:
	g++ -O2 name-generator.cpp -o name-generator_cpp_O2

name-generator_O1:
	gcc -O1 name-generator.c -o name-generator_O1

name-generator_cpp_O1:
	g++ -O1 name-generator.cpp -o name-generator_cpp_O1

rust: rust/target/debug/name-generator

rust/target/debug/name-generator:
	$(MAKE) -C rust all

name-generator_go:
	go build -o name-generator_go name-generator.go

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

NameGeneratorScala.class:
	scalac NameGeneratorScala.scala

SLOCCOUNT.md:
	sloccount ./ > SLOCCOUNT.md

data: docs/faster_scanner-20.csv docs/fastest_scanner-22.csv docs/fast_scanner-15.csv docs/scanner-11.csv docs/slow_scanner-5.csv docs/slowest_scanner-10.csv

docs/fastest_scanner-22.csv:
	SPEED=fastest_scanner SCAN_END=22 ./benchmark.sh
	cp log/fastest_scanner-22.csv docs/

docs/faster_scanner-20.csv:
	SPEED=faster_scanner SCAN_END=20 ./benchmark.sh
	cp log/faster_scanner-20.csv docs/

docs/fast_scanner-15.csv:
	SPEED=fast_scanner SCAN_END=15 ./benchmark.sh
	cp log/fast_scanner-15.csv docs/

docs/scanner-11.csv:
	SPEED=scanner SCAN_END=11 ./benchmark.sh
	cp log/scanner-11.csv docs/

docs/slow_scanner-5.csv:
	SPEED=slow_scanner SCAN_END=5 ./benchmark.sh
	cp log/slow_scanner-5.csv docs/

docs/slowest_scanner-10.csv:
	SPEED=slowest_scanner SCAN_END=10 ./benchmark.sh
	cp log/slowest_scanner-10.csv docs/
