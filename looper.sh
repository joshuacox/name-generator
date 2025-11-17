#!/usr/bin/env bash
: "${counto:=24}"
: ${TEST:="ponyc -b name-generator_pony"}
: ${MESSAGE:="/test ponyc -b name-generator_pony"}

countzero=0

$TEST
if [[ ! $? -eq 0 ]]; then
while [[ ${countzero} -lt ${counto} ]]; do
  aider -m ${MESSAGE}
  ((countzero++))
  $TEST
  if [[ $? -eq 0 ]]; then
    break
  fi
done
fi
