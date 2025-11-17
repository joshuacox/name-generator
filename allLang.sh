#!/usr/bin/env bash
# Define the CSV file to read
CSV_FILE="$(tail -n +2 langs.csv)"

if [[ $# -eq 1 ]]; then
  MESSAGE=$1
else
  echo 'useage:'
  echo "$0 message"
  exit 1
fi

aiderrr () {
  LANG_FILE=$1
  echo aider \
  --architect \
  --yes-always \
  --read "name-generator.sh" \
  --file "${LANG_FILE}" \
  --message "edit ${LANG_FILE} and ${MESSAGE}"
}

main () {
  # Loop through each line of the CSV file
  while IFS=, read -r LANG LANG_FILE; do
    aiderrr "${LANG_FILE}"
    #echo "${LANG_FILE} ${LANG}"
  done <<< "$CSV_FILE"

}

time main
