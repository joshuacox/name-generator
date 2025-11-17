#!/usr/bin/env bash
if [[ $# -eq 1 ]]; then
  NEW_LANG="$1"
  NEW_LANG_FILE="name-generator.$1"
elif [[ $# -eq 2 ]]; then
  NEW_LANG="$1"
  NEW_LANG_FILE="name-generator.$2"
else
  echo 'useage:'
  echo "$0 new-lang"
  echo 'or specify the extension:'
  echo "$0 new-lang new-lang-file-extension"
  exit 1
fi

aiderrr () {
  MESSAGE=$1
  aider \
  --architect \
  --yes-always \
  --read "name-generator.sh" \
  --file "${NEW_LANG_FILE}" \
  --message "$MESSAGE"
}

main () {
  touch ${NEW_LANG_FILE}
  chmod +x ${NEW_LANG_FILE}
  aiderrr "write ${NEW_LANG_FILE} in ${NEW_LANG} so that it behaves with SEPARATOR, NOUN_FILE and ADJ_FILE like the shellscript name-generator.sh does"
  aiderrr "can you add ${NEW_LANG} tests to the test/test.bats file?"

}

time main
