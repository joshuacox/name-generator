#!/usr/bin/env bats

@test "test name-generator at 10" {
  result="$(export counto=10; ./name-generator|wc -l)"
  [ "$result" -eq 10 ]
}
@test "test name-generator at 100" {
  result="$(export counto=100; ./name-generator|wc -l)"
  [ "$result" -eq 100 ]
}
@test "test NameGenerator at 10" {
  result="$(export counto=10; java NameGenerator|wc -l)"
  [ "$result" -eq 10 ]
}
@test "test NameGenerator at 100" {
  result="$(export counto=100; java NameGenerator|wc -l)"
  [ "$result" -eq 100 ]
}
@test "test name-generator_go at 10" {
  result="$(export counto=10; ./name-generator_go|wc -l)"
  [ "$result" -eq 10 ]
}
@test "test name-generator_go at 100" {
  result="$(export counto=100; ./name-generator_go|wc -l)"
  [ "$result" -eq 100 ]
}
@test "test name-generator.sh at 10" {
  result="$(export counto=10; ./name-generator.sh|wc -l)"
  [ "$result" -eq 10 ]
}
@test "test name-generator.sh at 100" {
  result="$(export counto=100; ./name-generator.sh|wc -l)"
  [ "$result" -eq 100 ]
}
@test "test name-generator.bash at 10" {
  result="$(export counto=10; ./name-generator.bash|wc -l)"
  [ "$result" -eq 10 ]
}
@test "test name-generator.bash at 100" {
  result="$(export counto=100; ./name-generator.bash|wc -l)"
  [ "$result" -eq 100 ]
}
@test "test name-generator.zsh at 10" {
  result="$(export counto=10; ./name-generator.zsh|wc -l)"
  [ "$result" -eq 10 ]
}
@test "test name-generator.zsh at 100" {
  result="$(export counto=100; ./name-generator.zsh|wc -l)"
  [ "$result" -eq 100 ]
}
@test "test name-generator.py at 10" {
  result="$(export counto=10; ./name-generator.py|wc -l)"
  [ "$result" -eq 10 ]
}
@test "test name-generator.py at 100" {
  result="$(export counto=100; ./name-generator.py|wc -l)"
  [ "$result" -eq 100 ]
}
@test "test name-generator.js at 10" {
  result="$(export counto=10; ./name-generator.js|wc -l)"
  [ "$result" -eq 10 ]
}
@test "test name-generator.js at 100" {
  result="$(export counto=100; ./name-generator.js|wc -l)"
  [ "$result" -eq 100 ]
}
@test "test name-generator-sync.js at 10" {
  result="$(export counto=10; ./name-generator-sync.js|wc -l)"
  [ "$result" -eq 10 ]
}
@test "test name-generator-sync.js at 100" {
  result="$(export counto=100; ./name-generator-sync.js|wc -l)"
  [ "$result" -eq 100 ]
}
@test "test $HOME/.cargo/target/debug/name-generator at 10" {
  result="$(export counto=10; $HOME/.cargo/target/debug/name-generator|wc -l)"
  [ "$result" -eq 10 ]
}
@test "test $HOME/.cargo/target/debug/name-generator at 100" {
  result="$(export counto=100; $HOME/.cargo/target/debug/name-generator|wc -l)"
  [ "$result" -eq 100 ]
}
