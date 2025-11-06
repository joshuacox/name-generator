#!/usr/bin/env bash
: ${counto:=12}
export counto=${counto}
set -eux
bench_runnr () {
  hyperfine \
    --warmup 2 \
    --runs 5 \
    './name-generator' \
    './name-generator_go' \
    './name-generator.sh' \
    './name-generator.bash' \
    './name-generator.zsh' \
    './name-generator.js' \
    './name-generator.rb' \
    'java NameGenerator' \
    'cabal run' \
    "$HOME/.cargo/target/debug/name-generator" \
    './name-generator-sync.js' \
    './name-generator.py'
}

main () {
bench_runnr
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
