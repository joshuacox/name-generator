#!/usr/bin/env bash
: ${counto:=12}
: "${SPEED:=normal}"
export counto=${counto}
set -eux
bench_runnr () {
  hyperfine \
    --warmup 2 \
    --runs 5 \
    --shell=none \
    './name-generator' \
    './name-generator_cpp' \
    './name-generator_go' \
    './name-generator.sh' \
    './name-generator.bash' \
    './name-generator.zsh' \
    './name-generator.js' \
    './name-generator.pl' \
    './name-generator.rb' \
    './name-generator.raku' \
    './name-generator.rkt' \
    'java NameGenerator' \
    'cabal run' \
    "$HOME/.cargo/target/debug/name-generator" \
    './name-generator-sync.js' \
    './name-generator.exs' \
    'erl -noshell -s name_generator name_generator -s init stop' \
    './name-generator.py'
}

fast_bench_runnr () {
  hyperfine \
    --warmup 1 \
    --runs 5 \
    --shell=none \
    './name-generator' \
    './name-generator_cpp' \
    './name-generator_go' \
    './name-generator.js' \
    './name-generator.pl' \
    'java NameGenerator' \
    'cabal run' \
    "$HOME/.cargo/target/debug/name-generator"
}

faster_bench_runnr () {
  hyperfine \
    --warmup 1 \
    --runs 2 \
    --shell=none \
    './name-generator_cpp' \
    './name-generator_go' \
    './name-generator.js' \
    './name-generator.pl' \
    "$HOME/.cargo/target/debug/name-generator"
}

fastest_bench_runnr () {
  hyperfine \
    --warmup 0 \
    --runs 1 \
    --shell=none \
    './name-generator_cpp' \
    './name-generator_go' \
    './name-generator.pl'
}

main () {
if [[ ${SPEED} == 'fast' ]]; then
  fast_bench_runnr
elif [[ ${SPEED} == 'faster' ]]; then
  faster_bench_runnr
elif [[ ${SPEED} == 'fastest' ]]; then
  fastest_bench_runnr
else
  bench_runnr
fi
}

time main

exit 0

# WIP
  # 'export counto={counto} ./name-generator.sh' \
  # 'export counto={counto} ./name-generator.bash' \
  # 'export counto={counto} ./name-generator.zsh' \
  # 'export counto={counto} ./name-generator.py'
  #--parameter-scan counto 100 1000 \
  #--parameter-step-size 100 \
  #'export ${counto} ${commands}'
  #--parameter-list commands ./name-generator.sh,./name-generator.bash,./name-generator.zsh,./name-generator.py \
  #--parameter-list commands ./name-generator.sh,./name-generator.bash,./name-generator.zsh,./name-generator.py \
