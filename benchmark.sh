#!/usr/bin/env bash
: ${counto:=12}
: "${SPEED:=normal}"
: ${SCAN_START:=1}
: ${SCAN_END:=15}
: ${HYPERFINE_LOGDIR:=log}
#: ${COMMON_HYPERFINE_OPTS:="counto={num_count} ./name-generator" "counto={num_count} ./name-generator_go" -P num_count 1 3 --export-csv log/timing.csv --export-json log/timing.json --export-asciidoc log/timing.ascii --export-markdown log/timing.md
#: ${COMMON_HYPERFINE_OPTS:='-P num_count 1 3'} --export-csv log/timing.csv --export-json log/timing.json --export-asciidoc log/timing.ascii --export-markdown log/timing.md
#: ${HYPERFINE_LOGS:=--export-csv log/timing.csv --export-json log/timing.json --export-asciidoc log/timing.ascii --export-markdown log/timing.md
export counto=${counto}
set -eux
mkdir -p log

slow_bench_runnr () {
  hyperfine \
    --export-csv ${HYPERFINE_LOGDIR}/timing-slow-${counto}.csv \
    --export-json ${HYPERFINE_LOGDIR}/timing-slow-${counto}.json \
    --export-asciidoc ${HYPERFINE_LOGDIR}/timing-slow-${counto}.adoc \
    --export-markdown ${HYPERFINE_LOGDIR}/timing-slow-${counto}.md \
    --warmup 1 \
    --runs 3 \
    --shell=none \
    './name-generator' \
    './name-generator_cpp' \
    './name-generator_O2' \
    './name-generator_cpp_O2' \
    './name-generator_O1' \
    './name-generator_cpp_O1' \
    './name-generator_go' \
    './name-generator_pascal' \
    './name-generator.sh' \
    './name-generator.fish' \
    './name-generator.bash' \
    './name-generator.zsh' \
    './name-generator.ts' \
    './name-generator.js' \
    './name-generator.jl' \
    'java -jar ./name-generator.jar' \
    './name-generator.kts' \
    './name-generator.pl' \
    './name-generator.m' \
    './name-generator.php' \
    './name-generator.el' \
    './name-generator.ml' \
    './name-generator.rb' \
    './name-generator.raku' \
    './name-generator.dart' \
    './name-generator.lua' \
    './name-generator.rkt' \
    'java NameGenerator' \
    'scala NameGeneratorScala' \
    'cabal run' \
    'rust/target/debug/name-generator' \
    './name-generator-sync.js' \
    './name-generator.exs' \
    'erl -noshell -s name_generator name_generator -s init stop' \
    './name-generator.py'
}

slowest_scanner_bench_runnr () {
  hyperfine \
    -P num_count ${SCAN_START} ${SCAN_END} \
    --export-csv ${HYPERFINE_LOGDIR}/${SPEED}-${SCAN_END}.csv \
    --export-json ${HYPERFINE_LOGDIR}/${SPEED}-${SCAN_END}.json \
    --export-asciidoc ${HYPERFINE_LOGDIR}/${SPEED}-${SCAN_END}.adoc \
    --export-markdown ${HYPERFINE_LOGDIR}/${SPEED}-${SCAN_END}.md \
    --warmup 1 \
    --runs 2 \
    --shell=bash \
    'counto={num_count} ./name-generator' \
    'counto={num_count} ./name-generator_cpp' \
    'counto={num_count} ./name-generator_O2' \
    'counto={num_count} ./name-generator_cpp_O2' \
    'counto={num_count} ./name-generator_O1' \
    'counto={num_count} ./name-generator_cpp_O1' \
    'counto={num_count} ./name-generator_go' \
    'counto={num_count} ./name-generator_pascal' \
    'counto={num_count} ./name-generator.sh' \
    'counto={num_count} ./name-generator.bash' \
    'counto={num_count} ./name-generator.fish' \
    'counto={num_count} ./name-generator.zsh' \
    'counto={num_count} ./name-generator.jl' \
    'counto={num_count} ./name-generator.el' \
    'counto={num_count} ./name-generator.pl' \
    'counto={num_count} ./name-generator.m' \
    'counto={num_count} ./name-generator.php' \
    'counto={num_count} ./name-generator.ml' \
    'counto={num_count} ./name-generator.rb' \
    'counto={num_count} ./name-generator.raku' \
    'counto={num_count} ./name-generator.rkt' \
    'counto={num_count} ./name-generator.dart' \
    'counto={num_count} ./name-generator.lua' \
    'counto={num_count} ./name-generator-sync.js' \
    'counto={num_count} ./name-generator.exs' \
    'counto={num_count} ./name-generator.ts' \
    'counto={num_count} ./name-generator.js' \
    'counto={num_count} java -jar ./name-generator.jar' \
    'counto={num_count} java NameGenerator' \
    'counto={num_count} scala NameGeneratorScala' \
    'counto={num_count} cabal run' \
    'counto={num_count} rust/target/debug/name-generator' \
    'counto={num_count} erl -noshell -s name_generator name_generator -s init stop' \
    'counto={num_count} ./name-generator.py'
}

slow_scanner_bench_runnr () {
  hyperfine \
    -P num_count ${SCAN_START} ${SCAN_END} \
    --export-csv ${HYPERFINE_LOGDIR}/${SPEED}-${SCAN_END}.csv \
    --export-json ${HYPERFINE_LOGDIR}/${SPEED}-${SCAN_END}.json \
    --export-asciidoc ${HYPERFINE_LOGDIR}/${SPEED}-${SCAN_END}.adoc \
    --export-markdown ${HYPERFINE_LOGDIR}/${SPEED}-${SCAN_END}.md \
    --warmup 1 \
    --runs 2 \
    --shell=bash \
    'counto=$((2**{num_count})) ./name-generator' \
    'counto=$((2**{num_count})) ./name-generator_cpp' \
    'counto=$((2**{num_count})) ./name-generator_O2' \
    'counto=$((2**{num_count})) ./name-generator_cpp_O2' \
    'counto=$((2**{num_count})) ./name-generator_O1' \
    'counto=$((2**{num_count})) ./name-generator_cpp_O1' \
    'counto=$((2**{num_count})) ./name-generator_go' \
    'counto=$((2**{num_count})) ./name-generator_pascal' \
    'counto=$((2**{num_count})) ./name-generator.sh' \
    'counto=$((2**{num_count})) ./name-generator.bash' \
    'counto=$((2**{num_count})) ./name-generator.fish' \
    'counto=$((2**{num_count})) ./name-generator.zsh' \
    'counto=$((2**{num_count})) ./name-generator.js' \
    'counto=$((2**{num_count})) ./name-generator.ts' \
    'counto=$((2**{num_count})) ./name-generator.jl' \
    'counto=$((2**{num_count})) java -jar ./name-generator.jar' \
    'counto=$((2**{num_count})) ./name-generator.el' \
    'counto=$((2**{num_count})) ./name-generator.pl' \
    'counto=$((2**{num_count})) ./name-generator.m' \
    'counto=$((2**{num_count})) ./name-generator.php' \
    'counto=$((2**{num_count})) ./name-generator.ml' \
    'counto=$((2**{num_count})) ./name-generator.rb' \
    'counto=$((2**{num_count})) ./name-generator.raku' \
    'counto=$((2**{num_count})) ./name-generator.rkt' \
    'counto=$((2**{num_count})) ./name-generator.dart' \
    'counto=$((2**{num_count})) ./name-generator.lua' \
    'counto=$((2**{num_count})) java NameGenerator' \
    'counto=$((2**{num_count})) scala NameGeneratorScala' \
    'counto=$((2**{num_count})) cabal run' \
    'counto=$((2**{num_count})) rust/target/debug/name-generator' \
    'counto=$((2**{num_count})) ./name-generator-sync.js' \
    'counto=$((2**{num_count})) ./name-generator.exs' \
    'counto=$((2**{num_count})) erl -noshell -s name_generator name_generator -s init stop' \
    'counto=$((2**{num_count})) ./name-generator.py'
}

scanner_bench_runnr () {
  hyperfine \
    -P num_count ${SCAN_START} ${SCAN_END} \
    --export-csv ${HYPERFINE_LOGDIR}/${SPEED}-${SCAN_END}.csv \
    --export-json ${HYPERFINE_LOGDIR}/${SPEED}-${SCAN_END}.json \
    --export-asciidoc ${HYPERFINE_LOGDIR}/${SPEED}-${SCAN_END}.adoc \
    --export-markdown ${HYPERFINE_LOGDIR}/${SPEED}-${SCAN_END}.md \
    --warmup 1 \
    --runs 2 \
    --shell=bash \
    'counto=$((2**{num_count})) ./name-generator' \
    'counto=$((2**{num_count})) ./name-generator_cpp' \
    'counto=$((2**{num_count})) ./name-generator_O2' \
    'counto=$((2**{num_count})) ./name-generator_cpp_O2' \
    'counto=$((2**{num_count})) ./name-generator_O1' \
    'counto=$((2**{num_count})) ./name-generator_cpp_O1' \
    'counto=$((2**{num_count})) ./name-generator_go' \
    'counto=$((2**{num_count})) ./name-generator_pascal' \
    'counto=$((2**{num_count})) ./name-generator.js' \
    'counto=$((2**{num_count})) ./name-generator.ts' \
    'counto=$((2**{num_count})) java -jar ./name-generator.jar' \
    'counto=$((2**{num_count})) ./name-generator.pl' \
    'counto=$((2**{num_count})) ./name-generator.m' \
    'counto=$((2**{num_count})) ./name-generator.php' \
    'counto=$((2**{num_count})) ./name-generator.ml' \
    'counto=$((2**{num_count})) ./name-generator.rb' \
    'counto=$((2**{num_count})) ./name-generator.lua' \
    'counto=$((2**{num_count})) java NameGenerator' \
    'counto=$((2**{num_count})) cabal run' \
    'counto=$((2**{num_count})) rust/target/debug/name-generator' \
    'counto=$((2**{num_count})) ./name-generator.exs' \
    'counto=$((2**{num_count})) ./name-generator.py'
}

fast_scanner_bench_runnr () {
  hyperfine \
    -P num_count ${SCAN_START} ${SCAN_END} \
    --export-csv ${HYPERFINE_LOGDIR}/${SPEED}-${SCAN_END}.csv \
    --export-json ${HYPERFINE_LOGDIR}/${SPEED}-${SCAN_END}.json \
    --export-asciidoc ${HYPERFINE_LOGDIR}/${SPEED}-${SCAN_END}.adoc \
    --export-markdown ${HYPERFINE_LOGDIR}/${SPEED}-${SCAN_END}.md \
    --warmup 1 \
    --runs 2 \
    --shell=bash \
    'counto=$((2**{num_count})) ./name-generator_cpp' \
    'counto=$((2**{num_count})) ./name-generator_cpp_O2' \
    'counto=$((2**{num_count})) ./name-generator_cpp_O1' \
    'counto=$((2**{num_count})) ./name-generator_go' \
    'counto=$((2**{num_count})) ./name-generator_pascal' \
    'counto=$((2**{num_count})) ./name-generator.js' \
    'counto=$((2**{num_count})) ./name-generator.ts' \
    'counto=$((2**{num_count})) java -jar ./name-generator.jar' \
    'counto=$((2**{num_count})) ./name-generator.pl' \
    'counto=$((2**{num_count})) ./name-generator.php' \
    'counto=$((2**{num_count})) ./name-generator.rb' \
    'counto=$((2**{num_count})) ./name-generator.lua' \
    'counto=$((2**{num_count})) java NameGenerator' \
    'counto=$((2**{num_count})) rust/target/debug/name-generator' \
    'counto=$((2**{num_count})) ./name-generator.py'
}

faster_scanner_bench_runnr () {
  hyperfine \
    -P num_count ${SCAN_START} ${SCAN_END} \
    --export-csv ${HYPERFINE_LOGDIR}/${SPEED}-${SCAN_END}.csv \
    --export-json ${HYPERFINE_LOGDIR}/${SPEED}-${SCAN_END}.json \
    --export-asciidoc ${HYPERFINE_LOGDIR}/${SPEED}-${SCAN_END}.adoc \
    --export-markdown ${HYPERFINE_LOGDIR}/${SPEED}-${SCAN_END}.md \
    --warmup 1 \
    --runs 2 \
    --shell=bash \
    'counto=$((2**{num_count})) ./name-generator_cpp' \
    'counto=$((2**{num_count})) ./name-generator_cpp_O2' \
    'counto=$((2**{num_count})) ./name-generator_cpp_O1' \
    'counto=$((2**{num_count})) ./name-generator_go' \
    'counto=$((2**{num_count})) ./name-generator_pascal' \
    'counto=$((2**{num_count})) ./name-generator.js' \
    'counto=$((2**{num_count})) ./name-generator.ts' \
    'counto=$((2**{num_count})) java -jar ./name-generator.jar' \
    'counto=$((2**{num_count})) ./name-generator.pl' \
    'counto=$((2**{num_count})) ./name-generator.php' \
    'counto=$((2**{num_count})) ./name-generator.rb' \
    'counto=$((2**{num_count})) ./name-generator.lua' \
    'counto=$((2**{num_count})) java NameGenerator' \
    'counto=$((2**{num_count})) rust/target/debug/name-generator' \
    'counto=$((2**{num_count})) ./name-generator.py'
}

fastest_scanner_bench_runnr () {
  hyperfine \
    -P num_count ${SCAN_START} ${SCAN_END} \
    --export-csv ${HYPERFINE_LOGDIR}/${SPEED}-${SCAN_END}.csv \
    --export-json ${HYPERFINE_LOGDIR}/${SPEED}-${SCAN_END}.json \
    --export-asciidoc ${HYPERFINE_LOGDIR}/${SPEED}-${SCAN_END}.adoc \
    --export-markdown ${HYPERFINE_LOGDIR}/${SPEED}-${SCAN_END}.md \
    --warmup 1 \
    --runs 2 \
    --shell=bash \
    'counto=$((2**{num_count})) ./name-generator_cpp' \
    'counto=$((2**{num_count})) ./name-generator_cpp_O2' \
    'counto=$((2**{num_count})) ./name-generator_cpp_O1' \
    'counto=$((2**{num_count})) ./name-generator_go' \
    'counto=$((2**{num_count})) ./name-generator_pascal' \
    'counto=$((2**{num_count})) ./name-generator.pl' \
    'counto=$((2**{num_count})) java NameGenerator' \
    'counto=$((2**{num_count})) ./name-generator.lua'
}

bench_runnr () {
  hyperfine \
    --warmup 3 \
    --runs 8 \
    --shell=none \
    --export-csv ${HYPERFINE_LOGDIR}/timing-${counto}.csv \
    --export-json ${HYPERFINE_LOGDIR}/timing-${counto}.json \
    --export-asciidoc ${HYPERFINE_LOGDIR}/timing-${counto}.adoc \
    --export-markdown ${HYPERFINE_LOGDIR}/timing-${counto}.md \
    './name-generator' \
    './name-generator_cpp' \
    './name-generator_O2' \
    './name-generator_cpp_O2' \
    './name-generator_O1' \
    './name-generator_cpp_O1' \
    './name-generator_go' \
    './name-generator_pascal' \
    './name-generator.zsh' \
    './name-generator.ts' \
    './name-generator.js' \
    './name-generator.jl' \
    './name-generator.pl' \
    './name-generator.m' \
    './name-generator.php' \
    './name-generator.rb' \
    './name-generator.raku' \
    './name-generator.rkt' \
    './name-generator.ml' \
    './name-generator.dart' \
    './name-generator.lua' \
    'java NameGenerator' \
    'java -jar ./name-generator.jar' \
    'cabal run' \
    'rust/target/debug/name-generator' \
    './name-generator-sync.js' \
    './name-generator.exs' \
    './name-generator.py'
}

fast_bench_runnr () {
  hyperfine \
    --warmup 3 \
    --runs 15 \
    --shell=none \
    --export-csv ${HYPERFINE_LOGDIR}/timing-fast-${counto}.csv \
    --export-json ${HYPERFINE_LOGDIR}/timing-fast-${counto}.json \
    --export-asciidoc ${HYPERFINE_LOGDIR}/timing-fast-${counto}.adoc \
    --export-markdown ${HYPERFINE_LOGDIR}/timing-fast-${counto}.md \
    './name-generator' \
    './name-generator_cpp' \
    './name-generator_O2' \
    './name-generator_cpp_O2' \
    './name-generator_O1' \
    './name-generator_cpp_O1' \
    './name-generator_go' \
    './name-generator_pascal' \
    './name-generator.lua' \
    './name-generator.js' \
    './name-generator.pl' \
    './name-generator.php' \
    'java NameGenerator' \
    'cabal run' \
    'rust/target/debug/name-generator'
}

faster_bench_runnr () {
  hyperfine \
    --warmup 1 \
    --runs 3 \
    --shell=none \
    --export-csv ${HYPERFINE_LOGDIR}/timing-faster-${counto}.csv \
    --export-json ${HYPERFINE_LOGDIR}/timing-faster-${counto}.json \
    --export-asciidoc ${HYPERFINE_LOGDIR}/timing-faster-${counto}.adoc \
    --export-markdown ${HYPERFINE_LOGDIR}/timing-faster-${counto}.md \
    './name-generator_cpp' \
    './name-generator_cpp_O2' \
    './name-generator_cpp_O1' \
    './name-generator_go' \
    './name-generator_pascal' \
    './name-generator.js' \
    './name-generator.lua' \
    './name-generator.pl' \
    './name-generator.php' \
    'java NameGenerator' \
    'rust/target/debug/name-generator'
}

fastest_bench_runnr () {
  hyperfine \
    --warmup 1 \
    --runs 2 \
    --shell=none \
    --export-csv ${HYPERFINE_LOGDIR}/timing-fastest-${counto}.csv \
    --export-json ${HYPERFINE_LOGDIR}/timing-fastest-${counto}.json \
    --export-asciidoc ${HYPERFINE_LOGDIR}/timing-fastest-${counto}.adoc \
    --export-markdown ${HYPERFINE_LOGDIR}/timing-fastest-${counto}.md \
    './name-generator_cpp' \
    './name-generator_cpp_O2' \
    './name-generator_cpp_O1' \
    './name-generator_go' \
    './name-generator_pascal' \
    './name-generator.lua' \
    'rust/target/debug/name-generator' \
    './name-generator.php' \
    'java NameGenerator' \
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
elif [[ ${SPEED} == 'slowest_scanner' ]]; then
  slowest_scanner_bench_runnr
elif [[ ${SPEED} == 'slow_scanner' ]]; then
  slow_scanner_bench_runnr
elif [[ ${SPEED} == 'fast_scanner' ]]; then
  fast_scanner_bench_runnr
elif [[ ${SPEED} == 'faster_scanner' ]]; then
  faster_scanner_bench_runnr
elif [[ ${SPEED} == 'fastest_scanner' ]]; then
  fastest_scanner_bench_runnr
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
