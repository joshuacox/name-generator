#!/usr/bin/env bash
set -eu
#zig build-exe -I . name-generator_zig.zig -lc
#Rscript name-generator.r
#bats -x test/r.bats
ponyc -b name-generator_pony
