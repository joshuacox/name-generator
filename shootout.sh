#!/usr/bin/env bash
: ${FILES:='name-generator.sh'}
: ${TEST:="make test"}
: ${MESSAGE:="/test ${TEST}"}
: ${MODELS_FILE:="models/models.list"}
: "${counto:=24}"

countzero=0

$TEST
if [[ ! $? -eq 0 ]]; then
for i in $(cat ${MODELS_FILE}); do
  echo "$i  <=======================" 
  aider \
    --config config/shootout.conf.yml \
    -m "${MESSAGE}"\
    --model "ollama_chat/$i" \
    ${FILES}
  ((countzero++))
  $TEST
  if [[ $? -eq 0 ]]; then
    break
  fi
  
done
fi
