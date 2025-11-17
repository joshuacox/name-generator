#!/usr/bin/env bash
: "${counto:=24}"
: ${TEST:="make test"}
: ${MESSAGE:="/test ${TEST}"}
: ${MODELS_FILE:="models/models.list"}

countzero=0

$TEST
if [[ ! $? -eq 0 ]]; then
for i in $(cat ${MODELS_FILE}); do
  echo "$i  <=======================" 
  aider \
    --config config/shootout.conf.yml \
    -m "${MESSAGE}"\
    --model "ollama_chat/$i"
  ((countzero++))
  $TEST
  if [[ $? -eq 0 ]]; then
    break
  fi
  
done
fi
