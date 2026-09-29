#!/usr/bin/env bash
set -eo pipefail

echo "============================================================"
echo "    Name Generator CI Benchmark & Data Aggregator (v2.0)"
echo "============================================================"

DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." >/dev/null 2>&1 && pwd)"
cd "$DIR"

mkdir -p log

echo "==> 1. Compiling available contenders..."

# Build C
if command -v gcc >/dev/null 2>&1; then
  echo "--> Compiling C (gcc -O3)..."
  gcc -O3 name-generator.c -o name-generator || true
fi

# Build C++
if command -v g++ >/dev/null 2>&1; then
  echo "--> Compiling C++ (g++ -O3)..."
  g++ -O3 name-generator.cpp -o name-generator_cpp || true
fi

# Build Go
if command -v go >/dev/null 2>&1; then
  echo "--> Compiling Go..."
  go build -o name-generator_go name-generator.go || true
fi

# Build Rust
if command -v cargo >/dev/null 2>&1; then
  echo "--> Compiling Rust (cargo build --release)..."
  cargo build --manifest-path rust/Cargo.toml --release || true
fi

# Build Zig
if command -v zig >/dev/null 2>&1; then
  echo "--> Compiling Zig (ReleaseFast)..."
  zig build-exe -O ReleaseFast -fstrip -femit-bin=name-generator_zig name-generator_zig.zig || true
fi

# Build Nim
if command -v nim >/dev/null 2>&1; then
  echo "--> Compiling Nim (release)..."
  nim c -d:release --opt:speed --hints:off -o:name-generator_nim name_generator.nim || true
fi

# Build Crystal
if command -v crystal >/dev/null 2>&1; then
  echo "--> Compiling Crystal (release)..."
  crystal build --release --no-debug -o name-generator_crystal name-generator.cr || true
fi

# Build Fortran
if command -v gfortran >/dev/null 2>&1; then
  echo "--> Compiling Fortran (gfortran -O3)..."
  gfortran -O3 -march=native -o name-generator_fortran name-generator.f90 || true
fi

# Build Ada
if command -v gnatmake >/dev/null 2>&1; then
  echo "--> Compiling Ada (gnatmake -O3)..."
  gnatmake -O3 name_generator_ada.adb -o name-generator_ada || true
fi

# Build Odin
if command -v odin >/dev/null 2>&1; then
  echo "--> Compiling Odin (-o:speed)..."
  odin build name-generator.odin -file -o:speed -out:name-generator_odin || true
fi

# Build V
if command -v v >/dev/null 2>&1; then
  echo "--> Compiling V (-prod)..."
  v -prod -o name-generator_v name-generator.v || true
fi

# Build COBOL
if command -v cobc >/dev/null 2>&1; then
  echo "--> Compiling COBOL (cobc -free -x -O3)..."
  cobc -free -x -O3 -o name-generator_cobol name-generator.cbl || true
fi

# Build Pascal
if command -v fpc >/dev/null 2>&1; then
  echo "--> Compiling Pascal (fpc -O3)..."
  fpc -O3 name-generator.pas -oname-generator_pascal >/dev/null 2>&1 || true
fi

# Build D
if command -v gdc >/dev/null 2>&1; then
  echo "--> Compiling D (gdc -O3)..."
  gdc -O3 name-generator_d.d -o name-generator_d || true
elif command -v dmd >/dev/null 2>&1; then
  echo "--> Compiling D (dmd)..."
  dmd -O -release -inline name-generator_d.d -of=name-generator_d || true
fi

# Build Dart
if command -v dart >/dev/null 2>&1; then
  echo "--> Compiling Dart (dart compile exe)..."
  dart compile exe name-generator.dart -o name-generator_dart >/dev/null 2>&1 || true
fi

# Build Java
if command -v javac >/dev/null 2>&1; then
  echo "--> Compiling Java (javac)..."
  javac NameGenerator.java || true
fi

echo "==> 2. Discovering available benchmark contenders..."

COMPILED_CANDIDATES=(
  "./name-generator_zig"
  "./name-generator"
  "./name-generator_cpp"
  "./name-generator_go"
  "rust/target/release/name-generator"
  "./name-generator_crystal"
  "./name-generator_nim"
  "./name-generator_odin"
  "./name-generator_ada"
  "./name-generator_cobol"
  "./name-generator_v"
  "./name-generator_fortran"
  "./name-generator_pascal"
  "./name-generator_d"
  "./name-generator_dart"
)

ACTIVE_COMPILED=()
for bin in "${COMPILED_CANDIDATES[@]}"; do
  if [[ -x "$bin" ]]; then
    if counto=1 "$bin" >/dev/null 2>&1; then
      ACTIVE_COMPILED+=("$bin")
    fi
  fi
done

VM_CANDIDATES=(
  "java NameGenerator"
)

ACTIVE_VM=()
for vm in "${VM_CANDIDATES[@]}"; do
  if counto=1 $vm >/dev/null 2>&1; then
    ACTIVE_VM+=("$vm")
  fi
done

SCRIPT_CANDIDATES=(
  "./name-generator.awk"
  "./name-generator.js"
  "./name-generator.ts"
  "./name-generator.py"
  "./name-generator.rb"
  "./name-generator.perl"
  "./name-generator.pl"
  "./name-generator.php"
  "./name-generator.lua"
  "./name-generator.tcl"
)

ACTIVE_SCRIPTS=()
for script in "${SCRIPT_CANDIDATES[@]}"; do
  if [[ -f "$script" ]]; then
    # Test if script can actually execute (interpreter is installed)
    if counto=1 "$script" >/dev/null 2>&1; then
      ACTIVE_SCRIPTS+=("$script")
    elif counto=1 perl "$script" >/dev/null 2>&1; then
      ACTIVE_SCRIPTS+=("perl $script")
    fi
  fi
done

SHELL_CANDIDATES=(
  "./name-generator.sh"
  "./name-generator.bash"
  "./name-generator.zsh"
  "./name-generator.fish"
  "./name-generator.ksh"
  "./name-generator.nu"
)

ACTIVE_SHELLS=()
for sh_cmd in "${SHELL_CANDIDATES[@]}"; do
  if [[ -f "$sh_cmd" ]]; then
    if counto=1 "$sh_cmd" >/dev/null 2>&1; then
      ACTIVE_SHELLS+=("$sh_cmd")
    fi
  fi
done

echo "Active compiled contenders (${#ACTIVE_COMPILED[@]}): ${ACTIVE_COMPILED[*]}"
echo "Active VM contenders (${#ACTIVE_VM[@]}): ${ACTIVE_VM[*]}"
echo "Active scripting contenders (${#ACTIVE_SCRIPTS[@]}): ${ACTIVE_SCRIPTS[*]}"
echo "Active shell contenders (${#ACTIVE_SHELLS[@]}): ${ACTIVE_SHELLS[*]}"

if command -v hyperfine >/dev/null 2>&1; then
  echo "==> 3. Running Hyperfine Tiered Deathmatches..."

  # 1. Compiled contenders deathmatch (counto=1000)
  if [[ ${#ACTIVE_COMPILED[@]} -gt 1 ]]; then
    echo "--> Running compiled contenders deathmatch (counto=1000)..."
    counto=1000 hyperfine \
      --warmup 2 \
      --runs 5 \
      --shell=none \
      --export-json log/ci-compiled.json \
      --export-markdown log/ci-compiled.md \
      "${ACTIVE_COMPILED[@]}" || true
  fi

  # 2. VM contenders deathmatch (counto=500)
  if [[ ${#ACTIVE_VM[@]} -gt 0 ]]; then
    echo "--> Running VM contenders deathmatch (counto=500)..."
    counto=500 hyperfine \
      --warmup 2 \
      --runs 5 \
      --shell=bash \
      --export-json log/ci-vm.json \
      "${ACTIVE_VM[@]}" || true
  fi

  # 3. Scripting contenders deathmatch (counto=100)
  if [[ ${#ACTIVE_SCRIPTS[@]} -gt 1 ]]; then
    echo "--> Running scripting contenders deathmatch (counto=100)..."
    counto=100 hyperfine \
      --warmup 1 \
      --runs 3 \
      --shell=none \
      --export-json log/ci-scripting.json \
      --export-markdown log/ci-scripting.md \
      "${ACTIVE_SCRIPTS[@]}" || true
  fi

  # 4. Shells contenders deathmatch (counto=50)
  if [[ ${#ACTIVE_SHELLS[@]} -gt 1 ]]; then
    echo "--> Running shells contenders deathmatch (counto=50)..."
    counto=50 hyperfine \
      --warmup 1 \
      --runs 3 \
      --shell=none \
      --export-json log/ci-shells.json \
      --export-markdown log/ci-shells.md \
      "${ACTIVE_SHELLS[@]}" || true
  fi

  # 5. Multi-scale scaling benchmarks
  SCALING_TARGETS=()
  for t in "./name-generator_zig" "./name-generator" "./name-generator_go" "./name-generator_crystal" "./name-generator_nim" "./name-generator_odin" "./name-generator_ada" "./name-generator_cobol" "./name-generator_pascal" "./name-generator_d" "./name-generator_dart" "java NameGenerator" "./name-generator.awk" "./name-generator.js" "python3 ./name-generator.py" "./name-generator.bash"; do
    if counto=1 $t >/dev/null 2>&1; then
      SCALING_TARGETS+=("counto={counto} $t")
    fi
  done

  if [[ ${#SCALING_TARGETS[@]} -gt 1 ]]; then
    echo "--> Running multi-scale scaling benchmarks (N=1,10,100,1000)..."
    hyperfine \
      -L counto 1,10,100,1000 \
      --runs 3 \
      --warmup 1 \
      --shell=bash \
      --export-json log/scaling-benchmarks.json \
      "${SCALING_TARGETS[@]}" || true
  fi
else
  echo "Hyperfine not found on system; skipping live execution and preserving stored baseline."
fi

echo "==> 4. Measuring Peak Memory (RSS) & Telemetry..."
python3 scripts/measure-telemetry.py || true

echo "==> 5. Ingesting benchmarks into Next.js dataset..."
node scripts/generate-data.mjs

echo "==> CI Benchmark run completed successfully!"
