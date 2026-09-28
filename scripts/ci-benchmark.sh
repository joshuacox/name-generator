#!/usr/bin/env bash
set -eo pipefail

echo "============================================================"
echo "    Name Generator CI Benchmark & Data Aggregator"
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

echo "==> 2. Discovering available benchmark contenders..."

COMPILED_CANDIDATES=(
  "./name-generator_zig"
  "./name-generator_go"
  "./name-generator_crystal"
  "./name-generator_nim"
  "./name-generator_odin"
  "./name-generator_ada"
  "./name-generator_cpp"
  "./name-generator_v"
  "./name-generator_fortran"
  "./name-generator"
  "rust/target/release/name-generator"
)

ACTIVE_COMPILED=()
for bin in "${COMPILED_CANDIDATES[@]}"; do
  if [[ -x "$bin" ]]; then
    ACTIVE_COMPILED+=("$bin")
  fi
done

SCRIPT_CANDIDATES=(
  "./name-generator.awk"
  "./name-generator.js"
  "./name-generator.rb"
  "./name-generator.tcl"
  "./name-generator.zsh"
  "./name-generator.sh"
  "./name-generator.bash"
  "./name-generator.ksh"
  "./name-generator.nu"
  "./name-generator.py"
  "./name-generator.pl"
  "./name-generator.php"
)

ACTIVE_SCRIPTS=()
for script in "${SCRIPT_CANDIDATES[@]}"; do
  if [[ -x "$script" ]]; then
    # Test if script can actually execute (interpreter is installed)
    if counto=1 "$script" >/dev/null 2>&1; then
      ACTIVE_SCRIPTS+=("$script")
    fi
  fi
done

echo "Active compiled contenders (${#ACTIVE_COMPILED[@]}): ${ACTIVE_COMPILED[*]}"
echo "Active scripting contenders (${#ACTIVE_SCRIPTS[@]}): ${ACTIVE_SCRIPTS[*]}"

if command -v hyperfine >/dev/null 2>&1; then
  echo "==> 3. Running Hyperfine Deathmatch Benchmarks..."

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
else
  echo "Hyperfine not found on system; skipping live execution and preserving stored baseline."
fi

echo "==> 4. Ingesting benchmarks into Next.js dataset..."
node scripts/generate-data.mjs

echo "==> CI Benchmark run completed successfully!"
