#!/usr/bin/env bash
: ${counto:=12}
: "${SPEED:=normal}"
: ${SCAN_START:=1}
: ${SCAN_END:=3}
: ${HYPERFINE_LOGDIR:=log}
#: ${COMMON_HYPERFINE_OPTS:="counto={num_count} ./name-generator" "counto={num_count} ./name-generator_go" -P num_count 1 3 --export-csv log/timing.csv --export-json log/timing.json --export-asciidoc log/timing.ascii --export-markdown log/timing.md
#: ${COMMON_HYPERFINE_OPTS:='-P num_count 1 3'} --export-csv log/timing.csv --export-json log/timing.json --export-asciidoc log/timing.ascii --export-markdown log/timing.md
#: ${HYPERFINE_LOGS:=--export-csv log/timing.csv --export-json log/timing.json --export-asciidoc log/timing.ascii --export-markdown log/timing.md
export counto=${counto}
set -eux

slow_bench_runnr () {
  hyperfine \
    --export-csv ${HYPERFINE_LOGDIR}/timing-slow-${counto}.csv \
    --export-json ${HYPERFINE_LOGDIR}/timing-slow-${counto}.json \
    --export-asciidoc ${HYPERFINE_LOGDIR}/timing-slow-${counto}.asciidoc \
    --export-markdown ${HYPERFINE_LOGDIR}/timing-slow-${counto}.md \
    --warmup 3 \
    --runs 5 \
    --shell=none \
    './name-generator' \
    './name-generator_cpp' \
    './name-generator_go' \
    './name-generator.sh' \
    './name-generator.bash' \
    './name-generator.zsh' \
    './name-generator.js' \
    './name-generator.jl' \
    'java -jar ./name-generator.jar' \
    './name-generator.kts' \
    './name-generator.pl' \
    './name-generator.el' \
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

scanner_bench_runnr () {
  hyperfine \
    -P num_count ${SCAN_START} ${SCAN_END} \
    --export-csv ${HYPERFINE_LOGDIR}/scanner-${counto}.csv \
    --export-json ${HYPERFINE_LOGDIR}/scanner-${counto}.json \
    --export-asciidoc ${HYPERFINE_LOGDIR}/scanner-${counto}.asciidoc \
    --export-markdown ${HYPERFINE_LOGDIR}/scanner-${counto}.md \
    --warmup 3 \
    --runs 5 \
    --shell=sh \
    'counto={num_count} ./name-generator' \
    'counto={num_count} ./name-generator_cpp' \
    'counto={num_count} ./name-generator_go' \
    'counto={num_count} ./name-generator.sh' \
    'counto={num_count} ./name-generator.bash' \
    'counto={num_count} ./name-generator.zsh' \
    'counto={num_count} ./name-generator.js' \
    'counto={num_count} ./name-generator.jl' \
    'counto={num_count} java -jar ./name-generator.jar' \
    'counto={num_count} ./name-generator.kts' \
    'counto={num_count} ./name-generator.pl' \
    'counto={num_count} ./name-generator.el' \
    'counto={num_count} ./name-generator.rb' \
    'counto={num_count} ./name-generator.raku' \
    'counto={num_count} ./name-generator.rkt' \
    'counto={num_count} java NameGenerator' \
    'counto={num_count} cabal run' \
    "counto={num_count} $HOME/.cargo/target/debug/name-generator" \
    'counto={num_count} ./name-generator-sync.js' \
    'counto={num_count} ./name-generator.exs' \
    'counto={num_count} erl -noshell -s name_generator name_generator -s init stop' \
    'counto={num_count} ./name-generator.py'
}

bench_runnr () {
  hyperfine \
    --warmup 3 \
    --runs 8 \
    --shell=none \
    --export-csv ${HYPERFINE_LOGDIR}/timing-${counto}.csv \
    --export-json ${HYPERFINE_LOGDIR}/timing-${counto}.json \
    --export-asciidoc ${HYPERFINE_LOGDIR}/timing-${counto}.asciidoc \
    --export-markdown ${HYPERFINE_LOGDIR}/timing-${counto}.md \
    './name-generator' \
    './name-generator_cpp' \
    './name-generator_go' \
    './name-generator.zsh' \
    './name-generator.js' \
    './name-generator.jl' \
    './name-generator.pl' \
    './name-generator.rb' \
    './name-generator.raku' \
    './name-generator.rkt' \
    'java NameGenerator' \
    'java -jar ./name-generator.jar' \
    'cabal run' \
    "$HOME/.cargo/target/debug/name-generator" \
    './name-generator-sync.js' \
    './name-generator.exs' \
    './name-generator.py'
}

fast_bench_runnr () {
  hyperfine \
    --warmup 1 \
    --runs 5 \
    --shell=none \
    --export-csv ${HYPERFINE_LOGDIR}/timing-fast-${counto}.csv \
    --export-json ${HYPERFINE_LOGDIR}/timing-fast-${counto}.json \
    --export-asciidoc ${HYPERFINE_LOGDIR}/timing-fast-${counto}.asciidoc \
    --export-markdown ${HYPERFINE_LOGDIR}/timing-fast-${counto}.md \
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
    --export-csv ${HYPERFINE_LOGDIR}/timing-faster-${counto}.csv \
    --export-json ${HYPERFINE_LOGDIR}/timing-faster-${counto}.json \
    --export-asciidoc ${HYPERFINE_LOGDIR}/timing-faster-${counto}.asciidoc \
    --export-markdown ${HYPERFINE_LOGDIR}/timing-faster-${counto}.md \
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
    --export-csv ${HYPERFINE_LOGDIR}/timing-fastest-${counto}.csv \
    --export-json ${HYPERFINE_LOGDIR}/timing-fastest-${counto}.json \
    --export-asciidoc ${HYPERFINE_LOGDIR}/timing-fastest-${counto}.asciidoc \
    --export-markdown ${HYPERFINE_LOGDIR}/timing-fastest-${counto}.md \
    './name-generator_cpp' \
    './name-generator_go' \
    './name-generator.pl'
}

main () {
if [[ ${SPEED} == 'fast' ]]; then
  fast_bench_runnr
elif [[ ${SPEED} == 'slow' ]]; then
  slow_bench_runnr
elif [[ ${SPEED} == 'faster' ]]; then
  faster_bench_runnr
elif [[ ${SPEED} == 'fastest' ]]; then
  fastest_bench_runnr
elif [[ ${SPEED} == 'scanner' ]]; then
  scanner_bench_runnr
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
