#!/usr/bin/env bash
set -eu
#zig build-exe -I . name-generator_zig.zig -lc
#Rscript name-generator.r
#bats -x test/r.bats
ponyc -b name-generator_pony
# rm name-generator_d
# set -eu
# dmd name-generator_d.d
# ./name-generator_d
# bats -x test/d.bats
#cd name_generator_gleam
#rm -rf build
#gleam run
#bats -x test/test.bats
