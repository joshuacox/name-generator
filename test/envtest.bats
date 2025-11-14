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
  export SEPARATOR=_
  result=$(./name-generator | tail -n1)
  assert_equal "$result" "test_test"
}
@test "test haskell test/test" {
  export counto=1 
  export NOUN_FILE=test/test 
  export ADJ_FILE=test/test 
  export SEPARATOR=_
  result=$(cabal run | tail -n1)
  assert_equal "$result" "test_test"
}
@test "test sh test/test" {
  export counto=1 
  export NOUN_FILE=test/test 
  export ADJ_FILE=test/test 
  export SEPARATOR=_
  result=$(./name-generator.sh | tail -n1)
  assert_equal "$result" "test_test"
}
@test "test bash test/test" {
  export counto=1 
  export NOUN_FILE=test/test 
  export ADJ_FILE=test/test 
  export SEPARATOR=_
  result=$(./name-generator.bash | tail -n1)
  assert_equal "$result" "test_test"
}
@test "test zsh test/test" {
  export counto=1 
  export NOUN_FILE=test/test 
  export ADJ_FILE=test/test 
  export SEPARATOR=_
  result=$(./name-generator.zsh | tail -n1)
  assert_equal "$result" "test_test"
}
