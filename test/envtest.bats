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
@test "test c test/test" {
  export counto=1 
  export NOUN_FILE=test/test 
  export ADJ_FILE=test/test 
  result=$(./name-generator | tail -n1)
  assert_equal "$result" "test-test"
}
@test "test haskell test/test" {
  export counto=1 
  export NOUN_FILE=test/test 
  export ADJ_FILE=test/test 
  result=$(cabal run | tail -n1)
  assert_equal "$result" "test-test"
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
@test "test zsh test/test" {
  export counto=1 
  export NOUN_FILE=test/test 
  export ADJ_FILE=test/test 
  result=$(./name-generator.zsh | tail -n1)
  assert_equal "$result" "test-test"
}
@test "test rust test/test" {
  export counto=1 
  export NOUN_FILE=test/test 
  export ADJ_FILE=test/test 
  result=$($HOME/.cargo/target/debug/name-generator)
  assert_equal "$result" "test-test"
}
@test "test go test/test" {
  export counto=1 
  export NOUN_FILE=test/test 
  export ADJ_FILE=test/test 
  result=$(./name-generator_go)
  assert_equal "$result" "test-test"
}
@test "test py test/test" {
  export counto=1 
  export NOUN_FILE=test/test 
  export ADJ_FILE=test/test 
  result=$(./name-generator.py)
  assert_equal "$result" "test-test"
}
@test "test js test/test" {
  export counto=1 
  export NOUN_FILE=test/test 
  export ADJ_FILE=test/test 
  result=$(./name-generator.js)
  assert_equal "$result" "test-test"
}
@test "test jssync test/test" {
  export counto=1 
  export NOUN_FILE=test/test 
  export ADJ_FILE=test/test 
  result=$(./name-generator-sync.js)
  assert_equal "$result" "test-test"
}
@test "test java test/test" {
  export counto=1 
  export NOUN_FILE=test/test 
  export ADJ_FILE=test/test 
  result=$(java NameGenerator)
  assert_equal "$result" "test-test"
}
