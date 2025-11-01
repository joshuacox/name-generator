#!/usr/bin/env zsh
HERE=$(pwd)
: "${counto:=$(tput lines)}"
: "${NOUN_FOLDER:=${HERE}/nouns}"
: "${ADJ_FOLDER:=${HERE}/adjectives}"
: "${NOUN_FILE:=$(realpath $(find ${NOUN_FOLDER} -type f | shuf -n 1))}"
: "${ADJ_FILE:=$(realpath $(find ${ADJ_FOLDER} -type f | shuf -n 1))}"
debugger () {
  if [[ ${DEBUG} == 'true' ]]; then
    set -x
    echo ${this_adjective}
    echo ${this_noun}
    echo $ADJ_FILE
    echo $ADJ_FOLDER
    echo $NOUN_FILE
    echo $NOUN_FOLDER
    echo "$countzero > $counto"
  fi
}
countzero=0
while [[ ${countzero} -lt ${counto} ]]; do
  this_noun=$(shuf -n 1 ${NOUN_FILE}| tr '[:upper:]' '[:lower:]')
  this_adjective=$(shuf -n 1 ${ADJ_FILE})
  this_name=$(printf "%s-%s" "${this_adjective}" "${this_noun}")
  debugger
  echo ${this_name}
  ((countzero++))
done
