#!/usr/bin/env bash
: "${counto:=24}"
: ${TEST:="./name-generator.r"}
: ${MESSAGE:="/test ${TEST}"}

countzero=0

$TEST
if [[ ! $? -eq 0 ]]; then
for i in $(cat models); do
  aider \
    --config .aider.shootout.conf.yml \
    -m "${MESSAGE}"\
    --model "ollama_chat/$i"
  ((countzero++))
  $TEST
  if [[ $? -eq 0 ]]; then
    break
  fi
  
done
fi
