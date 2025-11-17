#!/usr/bin/env bash
: "${counto:=24}"
: ${TEST:="ponyc -b name-generator_pony"}
: ${MESSAGE:="/test ponyc -b name-generator_pony"}

countzero=0

$TEST
if [[ ! $? -eq 0 ]]; then
for i in $(cat models); do
  echo aider \
    --env-file .aider .aider.shootout.conf.yml \
    -m "${MESSAGE}"\
    --model "$i"
  ((countzero++))
  $TEST
  if [[ $? -eq 0 ]]; then
    break
  fi
done
fi
