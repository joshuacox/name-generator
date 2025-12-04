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
