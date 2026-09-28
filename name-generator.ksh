#!/usr/bin/env ksh
# KornShell implementation of name-generator
#
# Behaves like `name-generator.sh`:
#   - Uses environment variables SEPARATOR, NOUN_FILE, ADJ_FILE,
#     NOUN_FOLDER, ADJ_FOLDER, counto, DEBUG.
#   - If NOUN_FILE / ADJ_FILE are not set, picks a random regular file
#     from the respective folder.
#   - Emits `counto` lines (default: terminal height via `tput lines`,
#     fallback 24).
#   - Noun is lower-cased, adjective keeps original case.
#   - Optional debug output when DEBUG=true.

HERE=$(pwd)
: "${SEPARATOR:="-"}"
if command -v tput >/dev/null 2>&1; then
  : "${counto:=$(tput lines 2>/dev/null)}"
fi
: "${counto:=24}"

: "${NOUN_FOLDER:=${HERE}/nouns}"
: "${ADJ_FOLDER:=${HERE}/adjectives}"
: "${NOUN_FILE:=$(realpath "$(find "${NOUN_FOLDER}" -type f | shuf -n 1)")}"
: "${ADJ_FILE:=$(realpath "$(find "${ADJ_FOLDER}" -type f | shuf -n 1)")}"

debugger () {
  if [[ ${DEBUG} == 'true' ]]; then
    set -x
    print "${this_adjective}"
    print "${this_noun}"
    print "${ADJ_FILE}"
    print "${ADJ_FOLDER}"
    print "${NOUN_FILE}"
    print "${NOUN_FOLDER}"
    print "${countzero} > ${counto}"
  fi
}

typeset -l this_noun
countzero=0
while (( countzero < counto )); do
  raw_noun=$(shuf -n 1 "${NOUN_FILE}" 2>/dev/null || sort -R "${NOUN_FILE}" | head -n 1)
  this_noun="${raw_noun}"
  this_adjective=$(shuf -n 1 "${ADJ_FILE}" 2>/dev/null || sort -R "${ADJ_FILE}" | head -n 1)

  debugger

  printf "%s%s%s\n" "${this_adjective}" "${SEPARATOR}" "${this_noun}"
  ((countzero++))
done
exit 0
