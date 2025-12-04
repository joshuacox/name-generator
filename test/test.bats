#!/usr/bin/env bats

setup() {
    load 'test_helper/bats-support/load'
    load 'test_helper/bats-assert/load'
    # ... the remaining setup is unchanged

    # get the containing directory of this file
    # use $BATS_TEST_FILENAME instead of ${BASH_SOURCE[0]} or $0,
    # as those will point to the bats executable's location or the preprocessed file respectively
    DIR="$( cd "$( dirname "$BATS_TEST_FILENAME" )" >/dev/null 2>&1 && pwd )"
    # make executables in src/ visible to PATH
    PATH="$DIR/../src:$PATH"
    export NOUN_FILE=test/test 
    export ADJ_FILE=test/test 
    export SEPARATOR=_
    export counto=1
}
@test "make all" {
  make clean
  make all
}

# D
@test "test name-generator_d at 10" {
  result="$(counto=10 ./name-generator_d|wc -l)"
  [ "$result" -eq 10 ]
}
@test "test name-generator_d at 13" {
  result="$(counto=13 ./name-generator_d|wc -l)"
  [ "$result" -eq 13 ]
}
@test "test D test/test" {
  result=$(./name-generator_d)
  assert_equal "$result" "test_test"
}
# R
@test "test name-generator.r at 10" {
  result="$(counto=10 ./name-generator.r|wc -l)"
  [ "$result" -eq 10 ]
}
@test "test name-generator.r at 13" {
  result="$(counto=13 ./name-generator.r|wc -l)"
  [ "$result" -eq 13 ]
}
@test "test R test/test" {
  result=$(./name-generator.r)
  assert_equal "$result" "test_test"
}
# octave
@test "test name-generator.m at 10" {
  result="$(counto=10 ./name-generator.m|wc -l)"
  [ "$result" -eq 10 ]
}
@test "test name-generator.m at 13" {
  result="$(counto=13 ./name-generator.m|wc -l)"
  [ "$result" -eq 13 ]
}
@test "test octave test/test" {
  result=$(./name-generator.m)
  assert_equal "$result" "test_test"
}
# pascal
@test "test name-generator_pascal at 10" {
  result="$(counto=10 ./name-generator_pascal|wc -l)"
  [ "$result" -eq 10 ]
}
@test "test name-generator_pascal at 13" {
  result="$(counto=13 ./name-generator_pascal|wc -l)"
  [ "$result" -eq 13 ]
}
@test "test pascal test/test" {
  result=$(./name-generator_pascal)
  assert_equal "$result" "test_test"
}
# fish
@test "test name-generator_fish at 10" {
  result="$(counto=10 ./name-generator.fish|wc -l)"
  [ "$result" -eq 10 ]
}
@test "test name-generator_fish at 13" {
  result="$(counto=13 ./name-generator.fish|wc -l)"
  [ "$result" -eq 13 ]
}
@test "test fish test/test" {
  result=$(./name-generator.fish)
  assert_equal "$result" "test_test"
}
# go
@test "test name-generator_go at 10" {
  result="$(counto=10 ./name-generator_go|wc -l)"
  [ "$result" -eq 10 ]
}
@test "test name-generator_go at 12399" {
  result="$(counto=12399 ./name-generator_go|wc -l)"
  [ "$result" -eq 12399 ]
}
@test "test go test/test" {
  result=$(./name-generator_go)
  assert_equal "$result" "test_test"
}
# raku
@test "test name-generator.raku at 10" {
  result="$(counto=10 ./name-generator.raku|wc -l)"
  [ "$result" -eq 10 ]
}
@test "test name-generator.raku at 73" {
  result="$(counto=73 ./name-generator.raku|wc -l)"
  [ "$result" -eq 73 ]
}
@test "test raku test/test" {
  result=$(./name-generator.raku)
  assert_equal "$result" "test_test"
}
# perl 
@test "test name-generator.pl at 10" {
  result="$(counto=10 ./name-generator.pl|wc -l)"
  [ "$result" -eq 10 ]
}
@test "test name-generator.pl at 75" {
  result="$(counto=75 ./name-generator.pl|wc -l)"
  [ "$result" -eq 75 ]
}
@test "test perl test/test" {
  result=$(./name-generator.pl)
  assert_equal "$result" "test_test"
}
# rust
@test "test 'rust/target/debug/name-generator' at 10" {
  result="$(counto=10 rust/target/debug/name-generator|wc -l)"
  [ "$result" -eq 10 ]
}
@test "test rust/target/debug/name-generator at 33433" {
  result="$(counto=33433 rust/target/debug/name-generator|wc -l)"
  [ "$result" -eq 33433 ]
}
@test "test rust test/test" {
  result="$(rust/target/debug/name-generator)"
  assert_equal "$result" "test_test"
}
# sh
@test "test name-generator.sh at 10" {
  result="$(counto=10 ./name-generator.sh|wc -l)"
  [ "$result" -eq 10 ]
}
@test "test name-generator.sh at 22" {
  result="$(counto=22 ./name-generator.sh|wc -l)"
  [ "$result" -eq 22 ]
}
@test "test sh test/test" {
  result=$(./name-generator.sh)
  assert_equal "$result" "test_test"
}
# bash
@test "test name-generator.bash at 10" {
  result="$(counto=10 ./name-generator.bash|wc -l)"
  [ "$result" -eq 10 ]
}
@test "test name-generator.bash at 33" {
  result="$(counto=33 ./name-generator.bash|wc -l)"
  [ "$result" -eq 33 ]
}
@test "test bash test/test" {
  echo $counto
  result=$(./name-generator.bash)
  assert_equal "$result" "test_test"
}
# ruby
@test "test name-generator.rb at 10" {
  result="$(counto=10 ./name-generator.rb|wc -l)"
  echo $result
  [ "$result" -eq 10 ]
}
@test "test name-generator.rb at 83" {
  result="$(counto=83 ./name-generator.rb|wc -l)"
  [ "$result" -eq 83 ]
}
@test "test ruby test/test" {
  result=$(./name-generator.rb)
  assert_equal "$result" "test_test"
}
# zsh
@test "test name-generator.zsh at 10" {
  result="$(counto=10 ./name-generator.zsh|wc -l)"
  [ "$result" -eq 10 ]
}
@test "test name-generator.zsh at 66" {
  result="$(counto=66 ./name-generator.zsh|wc -l)"
  [ "$result" -eq 66 ]
}
@test "test zsh test/test" {
  result=$(./name-generator.zsh)
  assert_equal "$result" "test_test"
}
# java
@test "test NameGenerator at 10" {
  result="$(counto=10 java NameGenerator|wc -l)"
  [ "$result" -eq 10 ]
}
@test "test NameGenerator at 55" {
  result="$(counto=55 java NameGenerator|wc -l)"
  [ "$result" -eq 55 ]
}
@test "test java test/test" {
  result=$(java NameGenerator)
  assert_equal "$result" "test_test"
}
# scala
@test "test NameGeneratorScala at 10" {
  result="$(counto=10 scala NameGeneratorScala|wc -l)"
  [ "$result" -eq 10 ]
}
@test "test NameGeneratorScala at 55" {
  result="$(counto=55 scala NameGeneratorScala|wc -l)"
  [ "$result" -eq 55 ]
}
@test "test scala test/test" {
  result=$(scala NameGeneratorScala)
  assert_equal "$result" "test_test"
}
# js sync
@test "test name-generator-sync.js at 10" {
  result="$(counto=10 ./name-generator-sync.js|wc -l)"
  [ "$result" -eq 10 ]
}
@test "test name-generator-sync.js at 99" {
  result="$(counto=99 ./name-generator-sync.js|wc -l)"
  [ "$result" -eq 99 ]
}
@test "test jssync test/test" {
  result=$(./name-generator-sync.js)
  assert_equal "$result" "test_test"
}
# js async
@test "test name-generator.js at 10" {
  result="$(counto=10 ./name-generator.js|wc -l)"
  [ "$result" -eq 10 ]
}
@test "test name-generator.js at 88" {
  result="$(counto=88 ./name-generator.js|wc -l)"
  [ "$result" -eq 88 ]
}
@test "test js test/test" {
  result=$(./name-generator.js)
  assert_equal "$result" "test_test"
}
# python
@test "test name-generator.py at 10" {
  result="$(counto=10 ./name-generator.py|wc -l)"
  [ "$result" -eq 10 ]
}
@test "test name-generator.py at 77" {
  result="$(counto=77 ./name-generator.py|wc -l)"
  [ "$result" -eq 77 ]
}
@test "test py test/test" {
  result=$(./name-generator.py)
  assert_equal "$result" "test_test"
}
# haskell
@test "test name-generator_haskell.hs at 10" {
  result="$(counto=10 cabal run|wc -l)"
  [ "$result" -eq 10 ]
}
@test "test name-generator_haskell.hs at 83" {
  result="$(counto=83 cabal run|wc -l)"
  [ "$result" -eq 83 ]
}
@test "test haskell test/test" {
  result=$(cabal run)
  assert_equal "$result" "test_test"
}
# c
@test "test name-generator at 10" {
  result="$(counto=10 ./name-generator|wc -l)"
  [ "$result" -eq 10 ]
}
@test "test name-generator at 44" {
  result="$(counto=44 ./name-generator|wc -l)"
  [ "$result" -eq 44 ]
}
@test "test c test/test" {
  result=$(./name-generator)
  assert_equal "$result" "test_test"
}
# cpp
@test "test name-generator_cpp at 10" {
  result="$(counto=10 ./name-generator_cpp|wc -l)"
  [ "$result" -eq 10 ]
}
@test "test name-generator_cpp at 48" {
  result="$(counto=48 ./name-generator_cpp|wc -l)"
  [ "$result" -eq 48 ]
}
@test "test cpp test/test" {
  result=$(./name-generator_cpp)
  assert_equal "$result" "test_test"
}
# erlang
@test "test name_generator.erl at 10" {
  result="$(counto=10 erl -noshell -s name_generator name_generator -s init stop|wc -l)"

  [ "$result" -eq 10 ]
}
@test "test name_generator.erl at 41" {
  result="$(counto=41 erl -noshell -s name_generator name_generator -s init stop|wc -l)"
  [ "$result" -eq 41 ]
}
@test "test erlang test/test" {
  result="$(erl -noshell -s name_generator name_generator -s init stop)"
  assert_equal "$result" "test_test"
}
# elixir
@test "test name-generator.exs at 10" {
  result="$(counto=10 ./name-generator.exs|wc -l)"
  [ "$result" -eq 10 ]
}
@test "test name-generator.exs at 49" {
  result="$(counto=49 ./name-generator.exs|wc -l)"
  [ "$result" -eq 49 ]
}
@test "test elixir test/test" {
  result=$(./name-generator.exs)
  assert_equal "$result" "test_test"
}
# racket
@test "test name-generator.rkt at 10" {
  result="$(counto=10 ./name-generator.rkt|wc -l)"
  [ "$result" -eq 10 ]
}
@test "test name-generator.rkt at 43" {
  result="$(counto=43 ./name-generator.rkt|wc -l)"
  [ "$result" -eq 43 ]
}
@test "test racket test/test" {
  result=$(./name-generator.rkt)
  assert_equal "$result" "test_test"
}
# julia
@test "test name-generator.jl at 10" {
  result="$(counto=10 ./name-generator.jl|wc -l)"
  [ "$result" -eq 10 ]
}
@test "test name-generator.jl at 83" {
  result="$(counto=83 ./name-generator.jl|wc -l)"
  [ "$result" -eq 83 ]
}
@test "test julia test/test" {
  result=$(./name-generator.jl)
  assert_equal "$result" "test_test"
}
# kotlin kts is slow
# @test "test kotlin name-generator.kts at 10" {
#   result="$(counto=10 ./name-generator.kts|wc -l)"
#   [ "$result" -eq 10 ]
# }
# @test "test kotlin name-generator.kts at 82" {
#   result="$(counto=82 ./name-generator.kts|wc -l)"
#   [ "$result" -eq 82 ]
# }
# @test "test kotlin kts test/test" {
#   result=$(./name-generator.kts)
#   assert_equal "$result" "test_test"
# }
# kotlin
@test "test kotlin name-generator.jar at 10" {
  result="$(counto=10 java -jar ./name-generator.jar|wc -l)"
  [ "$result" -eq 10 ]
}
@test "test name-generator.jl at 82" {
  result="$(counto=82 java -jar ./name-generator.jar|wc -l)"
  [ "$result" -eq 82 ]
}
@test "test kotlin kt test/test" {
  result="$(java -jar ./name-generator.jar)"
  assert_equal "$result" "test_test"
}
# elisp
@test "test elisp name-generator.el at 10" {
  result="$(counto=10 ./name-generator.el|wc -l)"
  [ "$result" -eq 10 ]
}
@test "test elisp name-generator.el at 22" {
  result="$(counto=22 ./name-generator.el|wc -l)"
  [ "$result" -eq 22 ]
}
@test "test elisp test/test" {
  result=$(./name-generator.el)
  assert_equal "$result" "test_test"
}
# dart
@test "test name-generator.dart at 10" {
  result="$(counto=10 ./name-generator.dart|wc -l)"
  [ "$result" -eq 10 ]
}
@test "test name-generator.dart at 57" {
  result="$(counto=57 ./name-generator.dart|wc -l)"
  [ "$result" -eq 57 ]
}
@test "test dart test/test" {
  result=$(./name-generator.dart)
  assert_equal "$result" "test_test"
}
# lua
@test "test name_generator.lua at 10" {
  result="$(counto=10 ./name-generator.lua | wc -l)"
  [ "$result" -eq 10 ]
}
@test "test name_generator.lua at 33" {
  result="$(counto=33 ./name-generator.lua | wc -l)"
  [ "$result" -eq 33 ]
}
@test "test lua test/test" {
  result=$(./name-generator.lua)
  assert_equal "$result" "test_test"
}
# OCaml implementation (name-generator.ml)
@test "test name-generator.ml at 10" {
  result="$(counto=10 ./name-generator.ml | wc -l)"
  [ "$result" -eq 10 ]
}
@test "test name-generator.ml at 73" {
  result="$(counto=73 ./name-generator.ml | wc -l)"
  [ "$result" -eq 73 ]
}
@test "test ocaml test/test" {
  result=$(./name-generator.ml)
  assert_equal "$result" "test_test"
}
# php implementation (name-generator.php)
@test "test name-generator.php at 10" {
  result="$(counto=10 ./name-generator.php | wc -l)"
  [ "$result" -eq 10 ]
}
@test "test name-generator.php at 73" {
  result="$(counto=73 ./name-generator.php | wc -l)"
  [ "$result" -eq 73 ]
}
@test "test php test/test" {
  result="$(./name-generator.php)"
  assert_equal "$result" "test_test"
}
# ts implementation (name-generator.ts)
@test "test name-generator.ts at 10" {
  result="$(counto=10 ./name-generator.ts | wc -l)"
  [ "$result" -eq 10 ]
}
@test "test name-generator.ts at 33" {
  result="$(counto=33 ./name-generator.ts | wc -l)"
  [ "$result" -eq 33 ]
}
@test "test ts test/test" {
  result="$(./name-generator.ts)"
  assert_equal "$result" "test_test"
}
