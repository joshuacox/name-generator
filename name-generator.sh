#!/bin/sh
HERE=$(pwd)
: "${NOUN_FOLDER:=${HERE}/nouns}"
: "${ADJ_FOLDER:=${HERE}/adjectives}"
: "${NOUN_FILE:=$(realpath $(find ${NOUN_FOLDER} -type f | shuf -n 1))}"
: "${ADJ_FILE:=$(realpath $(find ${ADJ_FOLDER} -type f | shuf -n 1))}"
this_noun=$(shuf -n 1 ${NOUN_FILE}| tr '[:upper:]' '[:lower:]')
this_adjective=$(shuf -n 1 ${ADJ_FILE})
if [[ ${DEBUG} == 'true' ]]; then
set -x
echo ${this_adjective}
echo ${this_noun}
echo $ADJ_FILE
echo $ADJ_FOLDER
echo $NOUN_FILE
echo $NOUN_FOLDER
fi
this_name=$(printf "%s-%s" "${this_adjective}" "${this_noun}")
echo ${this_name}
