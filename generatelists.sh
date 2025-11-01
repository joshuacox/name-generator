#!/usr/bin/env bash
set -eux
cut -f2 -d, data/nounlist.csv > alternative-nouns/long.list
cut -f2 -d, data/Adjectives.csv \
  |tr -d '\r' \
  |sed 's/^\s*\(\S*\)/\1/' \
  |sed 's/\s*$//' \
  |grep -v '^Word$' \
  |grep -v '^a$' \
  > alternative-adjectives/long.list
cut -f1 -d, data/greek_gods.csv > alternative-nouns/greek_gods.list
cut -f2 -d, data/presidents.csv|grep -v '^name$'|sed 's/.*\s\(\S*\)/\1/'|sort|uniq > alternative-nouns/us_presidents.list
cat alternative-nouns/*.list > nouns/full.list
cat alternative-adjectives/*.list > adjectives/full.list
