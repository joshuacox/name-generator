#!/usr/bin/env bash
set -eux
cut -f2 -d, data/nounlist.csv > ../alternative-nouns/long.list
cut -f2 -d, data/Adjectives.csv > alternative-adjectives/long.list
cut -f1 -d, data/greek_gods.csv > alternative-nouns/greek_gods.list
cat alternative-nouns/*.list > nouns/full.list
cat alternative-adjectives/*.list > adjectives/full.list
