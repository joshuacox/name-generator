#!/usr/bin/env bash
HERE=$(pwd)
: "${NOUN_FOLDER:=${HERE}/nouns}"
: "${ADJ_FOLDER:=${HERE}/adjectives}"
: "${NOUN_FILE:=$(realpath $(find ${NOUN_FOLDER} -type f | shuf -n 1))}"
: "${ADJ_FILE:=$(realpath $(find ${ADJ_FOLDER} -type f | shuf -n 1))}"
noun=$(shuf -n 1 ${NOUN_FILE}| tr '[:upper:]' '[:lower:]')
adjective=$(shuf -n 1 ${ADJ_FILE})
echo ${adjective}-${noun}
