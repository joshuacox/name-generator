#!/usr/bin/env bash
#: ${FILES:='name-generator.sh'}
#: ${TEST:="make test"}
: ${FILES:='name-generator.fish'}
: ${TEST:="./name-generator.fish"}
: ${MESSAGE:="/test ${TEST}"}
: "${counto:=24}"

countzero=0

$TEST
if [[ ! $? -eq 0 ]]; then
while [[ ${countzero} -lt ${counto} ]]; do
  aider -m "${MESSAGE}" ${FILES}
  ((countzero++))
  $TEST
  if [[ $? -eq 0 ]]; then
    break
  fi
done
fi
