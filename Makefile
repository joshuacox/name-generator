.PHONY: all test testx homepage github commit rust data web web-install web-bench web-build web-dev

all: name-generator name-generator_cpp name-generator_go NameGenerator.class name_generator.beam name-generator.jar rust/target/debug/name-generator name-generator_O2 name-generator_cpp_O2 name-generator_O1 name-generator_cpp_O1 NameGeneratorScala.class name-generator_pascal name-generator_d name-generator_nim name-generator_crystal name-generator_zig name-generator_fortran name-generator_ada name-generator_odin name-generator_v name-generator_cobol name-generator_dart

clean:
	-@rm -v name-generator 
	-@rm -v name-generator_cpp
	-@rm -v rust/target/debug/name-generator
	-@rm -v NameGenerator.class
	-@rm -v name-generator_go
	-@rm -v name_generator.beam
	-@rm -v NameGeneratorScala.class
	-@rm -v name-generator_pascal
	-@rm -v name-generator_d
	-@rm -v name-generator_nim
	-@rm -v name-generator_crystal
	-@rm -v name-generator_zig
	-@rm -v name-generator_fortran
	-@rm -v name-generator_ada
	-@rm -v name-generator_odin
	-@rm -v name-generator_v
	-@rm -v name-generator_cobol
	-@rm -v name-generator_dart
	-@rm -v name_generator_ada.ali name_generator_ada.o name-generator.o

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

web-install:
	cd web && npm install

web-bench:
	./scripts/ci-benchmark.sh

web-build: web-bench
	cd web && npm run build

web-dev:
	cd web && npm run dev

web: web-build

	./meta-benchmark.sh | tee BENCHMARK.md

benchmark.cast:
	time asciinema rec --command "./meta-benchmark.sh" benchmark.cast

name-generator.jar:
	kotlinc name-generator.kt -include-runtime -d name-generator.jar

commit:
	aider --commit --model=ollama_chat/llama3.2

NameGeneratorScala.class:
	scalac NameGeneratorScala.scala

SLOCCOUNT.md:
	sloccount ./ > SLOCCOUNT.md

clean-data: 
	rm -v docs/faster_scanner-20.csv docs/fastest_scanner-24.csv docs/fast_scanner-15.csv docs/scanner-11.csv docs/slow_scanner-5.csv docs/slowest_scanner-10.csv

data: docs/faster_scanner-20.csv docs/fastest_scanner-24.csv docs/fast_scanner-15.csv docs/scanner-11.csv docs/slow_scanner-5.csv docs/slowest_scanner-10.csv

docs/fastest_scanner-24.csv:
	SPEED=fastest_scanner SCAN_END=24 ./benchmark.sh
	cp log/fastest_scanner-24.csv docs/

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

name-generator_pascal:
	fpc -O3 name-generator.pas -oname-generator_pascal

name-generator_d:
	@if command -v dmd >/dev/null 2>&1; then dmd -O -release -inline name-generator_d.d -of=name-generator_d; \
	elif command -v gdc >/dev/null 2>&1; then gdc -O3 name-generator_d.d -o name-generator_d; \
	elif command -v ldc2 >/dev/null 2>&1; then ldc2 -O3 -release name-generator_d.d -of=name-generator_d; fi

name-generator_dart:
	dart compile exe name-generator.dart -o name-generator_dart

name-generator_nim:
	nim c -d:release --opt:speed --hints:off -o:name-generator_nim name_generator.nim

name-generator_crystal:
	crystal build --release --no-debug -o name-generator_crystal name-generator.cr

name-generator_zig:
	zig build-exe -O ReleaseFast -fstrip -femit-bin=name-generator_zig name-generator_zig.zig

name-generator_fortran:
	gfortran -O3 -march=native -o name-generator_fortran name-generator.f90

name-generator_ada:
	gnatmake -O3 name_generator_ada.adb -o name-generator_ada

name-generator_odin:
	odin build name-generator.odin -file -o:speed -out:name-generator_odin

name-generator_v:
	v -prod -o name-generator_v name-generator.v

name-generator_cobol:
	cobc -free -x -O3 -o name-generator_cobol name-generator.cbl

# WIPs
#
name-generator_pony:
	ponyc -b name-generator_pony

name_generator.beam:
	erl -compile name_generator
