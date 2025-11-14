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
}

@test "test name-generator at 10" {
  result="$(export counto=10; ./name-generator|wc -l)"
  [ "$result" -eq 10 ]
}
@test "test name-generator at 44" {
  result="$(export counto=44; ./name-generator|wc -l)"
  [ "$result" -eq 44 ]
}
@test "test NameGenerator at 10" {
  result="$(export counto=10; java NameGenerator|wc -l)"
  [ "$result" -eq 10 ]
}
@test "test NameGenerator at 55" {
  result="$(export counto=55; java NameGenerator|wc -l)"
  [ "$result" -eq 55 ]
}
@test "test name-generator_go at 10" {
  result="$(export counto=10; ./name-generator_go|wc -l)"
  [ "$result" -eq 10 ]
}
@test "test name-generator_go at 12399" {
  result="$(export counto=12399; ./name-generator_go|wc -l)"
  [ "$result" -eq 12399 ]
}
@test "test name-generator.sh at 10" {
  result="$(export counto=10; ./name-generator.sh|wc -l)"
  [ "$result" -eq 10 ]
}
@test "test name-generator.sh at 22" {
  result="$(export counto=22; ./name-generator.sh|wc -l)"
  [ "$result" -eq 22 ]
}
@test "test name-generator.bash at 10" {
  result="$(export counto=10; ./name-generator.bash|wc -l)"
  [ "$result" -eq 10 ]
}
@test "test name-generator.bash at 33" {
  result="$(export counto=33; ./name-generator.bash|wc -l)"
  [ "$result" -eq 33 ]
}
@test "test name-generator.zsh at 10" {
  result="$(export counto=10; ./name-generator.zsh|wc -l)"
  [ "$result" -eq 10 ]
}
@test "test name-generator.zsh at 66" {
  result="$(export counto=66; ./name-generator.zsh|wc -l)"
  [ "$result" -eq 66 ]
}
@test "test name-generator.py at 10" {
  result="$(export counto=10; ./name-generator.py|wc -l)"
  [ "$result" -eq 10 ]
}
@test "test name-generator.py at 77" {
  result="$(export counto=77; ./name-generator.py|wc -l)"
  [ "$result" -eq 77 ]
}
@test "test name-generator.pl at 10" {
  result="$(export counto=10; ./name-generator.pl|wc -l)"
  [ "$result" -eq 10 ]
}
@test "test name-generator.pl at 75" {
  result="$(export counto=75; ./name-generator.pl|wc -l)"
  [ "$result" -eq 75 ]
}
@test "test name-generator.js at 10" {
  result="$(export counto=10; ./name-generator.js|wc -l)"
  [ "$result" -eq 10 ]
}
@test "test name-generator.js at 88" {
  result="$(export counto=88; ./name-generator.js|wc -l)"
  [ "$result" -eq 88 ]
}
@test "test name-generator-sync.js at 10" {
  result="$(export counto=10; ./name-generator-sync.js|wc -l)"
  [ "$result" -eq 10 ]
}
@test "test name-generator-sync.js at 99" {
  result="$(export counto=99; ./name-generator-sync.js|wc -l)"
  [ "$result" -eq 99 ]
}
@test "test $HOME/.cargo/target/debug/name-generator at 10" {
  result="$(export counto=10; $HOME/.cargo/target/debug/name-generator|wc -l)"
  [ "$result" -eq 10 ]
}
@test "test $HOME/.cargo/target/debug/name-generator at 33433" {
  result="$(export counto=33433; $HOME/.cargo/target/debug/name-generator|wc -l)"
  [ "$result" -eq 33433 ]
}
@test "test name-generator.rb at 10" {
  result="$(export counto=10; ./name-generator.rb|wc -l)"
  echo $result
  [ "$result" -eq 10 ]
}
@test "test name-generator.rb at 83" {
  result="$(export counto=83; ./name-generator.rb|wc -l)"
  [ "$result" -eq 83 ]
}
@test "test name-generator_haskell.hs at 10" {
  result="$(export counto=10; cabal run|wc -l)"
  echo $result
  [ "$result" -eq 10 ]
}
@test "test name-generator_haskell.hs at 83" {
  result="$(export counto=83; cabal run|wc -l)"
  [ "$result" -eq 83 ]
}
@test "test sh test/test" {
  export counto=1 
  export NOUN_FILE=test/test 
  export ADJ_FILE=test/test 
  result=$(./name-generator.sh | tail -n1)
  assert_equal "$result" "test-test"
}
@test "test bash test/test" {
  export counto=1 
  export NOUN_FILE=test/test 
  export ADJ_FILE=test/test 
  result=$(./name-generator.bash | tail -n1)
  assert_equal "$result" "test-test"
}
@test "test perl test/test" {
  export counto=1 
  export NOUN_FILE=test/test 
  export ADJ_FILE=test/test 
  result=$(./name-generator.pl | tail -n1)
  assert_equal "$result" "test-test"
}
@test "test ruby test/test" {
  export counto=1 
  export NOUN_FILE=test/test 
  export ADJ_FILE=test/test 
  result=$(./name-generator.rb | tail -n1)
  assert_equal "$result" "test-test"
}
@test "test zsh test/test" {
  export counto=1 
  export NOUN_FILE=test/test 
  export ADJ_FILE=test/test 
  result=$(./name-generator.zsh | tail -n1)
  assert_equal "$result" "test-test"
}
@test "test go test/test" {
  export counto=1 
  export NOUN_FILE=test/test 
  export ADJ_FILE=test/test 
  result=$(./name-generator_go)
  assert_equal "$result" "test-test"
}
@test "test java test/test" {
  export counto=1 
  export NOUN_FILE=test/test 
  export ADJ_FILE=test/test 
  result=$(java NameGenerator)
  assert_equal "$result" "test-test"
}
