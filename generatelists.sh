#!/usr/bin/env bash
set -eux
# long noun list
cut -f2 -d, data/nounlist.csv > alternative-nouns/long.list
# long adj list
cut -f2 -d, data/Adjectives.csv \
  |tr -d '\r' \
  |sed 's/^\s*\(\S*\)/\1/' \
  |sed 's/\s*$//' \
  |tr ' ' '-' \
  |grep -v '^Word$' \
  |grep -v '^a$' \
  > alternative-adjectives/long.list
# greek gods
cut -f1 -d, data/greek_gods.csv > alternative-nouns/greek_gods.list
# presidents
cut -f2 -d, data/presidents.csv \
  |grep -v '^name$'|sed 's/.*\s\(\S*\)/\1/' \
  |sort|uniq \
  > alternative-nouns/us_presidents.list
# dog names
cut -f1 -d, data/dogNames2.csv \
  |grep -v -P '^\d+$'\
  |grep -v '?'\
  |grep -v '-'\
  |grep -v '('\
  |grep -v ')'\
  |grep -v '^\S$'\
  |grep -v '^\S\S$'\
  |grep -v '^\S\S\S$' \
  |grep -v '^\S\S\S\S$' \
  > alternative-nouns/dog_names.list
# drug names are not so good commenting for now
# cat DrugNames.txt|sort|uniq|tr ' ' '-'|tr '/' '-'

# congolmerate
cat alternative-nouns/*.list      |sort|uniq > nouns/full.list
cat alternative-adjectives/*.list |sort|uniq > adjectives/full.list
