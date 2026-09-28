import fs from 'node:fs';
import path from 'node:path';
import { fileURLToPath } from 'node:url';

const __dirname = path.dirname(fileURLToPath(import.meta.url));
const ROOT_DIR = path.resolve(__dirname, '..');
const WEB_DATA_DIR = path.resolve(ROOT_DIR, 'web/src/data');
const WEB_PUBLIC_DATA_DIR = path.resolve(ROOT_DIR, 'web/public/data');
const LOG_DIR = path.resolve(ROOT_DIR, 'log');

fs.mkdirSync(WEB_DATA_DIR, { recursive: true });
fs.mkdirSync(WEB_PUBLIC_DATA_DIR, { recursive: true });

// Language registry and categorization
const LANGUAGES_META = [
  { id: 'zig', name: 'Zig', file: 'name-generator_zig.zig', category: 'Compiled', paradigm: 'Systems', ext: '.zig', compiledBin: 'name-generator_zig' },
  { id: 'crystal', name: 'Crystal', file: 'name-generator.cr', category: 'Compiled', paradigm: 'Object-Oriented', ext: '.cr', compiledBin: 'name-generator_crystal' },
  { id: 'nim', name: 'Nim', file: 'name_generator.nim', category: 'Compiled', paradigm: 'Multi-paradigm', ext: '.nim', compiledBin: 'name-generator_nim' },
  { id: 'fortran', name: 'Fortran', file: 'name-generator.f90', category: 'Compiled', paradigm: 'Scientific / Procedural', ext: '.f90', compiledBin: 'name-generator_fortran' },
  { id: 'ada', name: 'Ada', file: 'name_generator_ada.adb', category: 'Compiled', paradigm: 'Safety-critical / Structured', ext: '.adb', compiledBin: 'name-generator_ada' },
  { id: 'odin', name: 'Odin', file: 'name-generator.odin', category: 'Compiled', paradigm: 'Data-oriented Systems', ext: '.odin', compiledBin: 'name-generator_odin' },
  { id: 'v', name: 'V', file: 'name-generator.v', category: 'Compiled', paradigm: 'Simple Systems', ext: '.v', compiledBin: 'name-generator_v' },
  { id: 'c', name: 'C', file: 'name-generator.c', category: 'Compiled', paradigm: 'Procedural Systems', ext: '.c', compiledBin: 'name-generator' },
  { id: 'cpp', name: 'C++', file: 'name-generator.cpp', category: 'Compiled', paradigm: 'Object-Oriented Systems', ext: '.cpp', compiledBin: 'name-generator_cpp' },
  { id: 'go', name: 'Go', file: 'name-generator.go', category: 'Compiled', paradigm: 'Concurrent Systems', ext: '.go', compiledBin: 'name-generator_go' },
  { id: 'rust', name: 'Rust', file: 'rust/src/main.rs', category: 'Compiled', paradigm: 'Safe Systems', ext: '.rs', compiledBin: 'rust/target/debug/name-generator' },
  { id: 'd', name: 'D', file: 'name-generator_d.d', category: 'Compiled', paradigm: 'Multi-paradigm Systems', ext: '.d', compiledBin: 'name-generator_d' },
  { id: 'pascal', name: 'Pascal', file: 'name-generator.pas', category: 'Compiled', paradigm: 'Structured', ext: '.pas', compiledBin: 'name-generator_pascal' },
  { id: 'haskell', name: 'Haskell', file: 'name-generator_haskell.hs', category: 'Compiled', paradigm: 'Pure Functional', ext: '.hs' },
  { id: 'ocaml', name: 'OCaml', file: 'name-generator.ml', category: 'Compiled', paradigm: 'Functional / Impure', ext: '.ml' },

  { id: 'awk', name: 'AWK', file: 'name-generator.awk', category: 'Scripting', paradigm: 'Pattern Scanning & Processing', ext: '.awk' },
  { id: 'ksh', name: 'KornShell (ksh)', file: 'name-generator.ksh', category: 'Shell', paradigm: 'POSIX Shell', ext: '.ksh' },
  { id: 'tcl', name: 'Tcl', file: 'name-generator.tcl', category: 'Scripting', paradigm: 'Command Language / String', ext: '.tcl' },
  { id: 'nu', name: 'Nushell', file: 'name-generator.nu', category: 'Shell', paradigm: 'Structured Data Shell', ext: '.nu' },
  { id: 'bash', name: 'Bash', file: 'name-generator.bash', category: 'Shell', paradigm: 'Unix Shell', ext: '.bash' },
  { id: 'zsh', name: 'Zsh', file: 'name-generator.zsh', category: 'Shell', paradigm: 'Extended Unix Shell', ext: '.zsh' },
  { id: 'sh', name: 'POSIX sh', file: 'name-generator.sh', category: 'Shell', paradigm: 'Bourne Shell', ext: '.sh' },
  { id: 'fish', name: 'Fish', file: 'name-generator.fish', category: 'Shell', paradigm: 'Friendly Interactive Shell', ext: '.fish' },

  { id: 'javascript', name: 'JavaScript (Node.js)', file: 'name-generator.js', category: 'Scripting', paradigm: 'Event-driven Async', ext: '.js' },
  { id: 'typescript', name: 'TypeScript', file: 'name-generator.ts', category: 'Scripting', paradigm: 'Typed JavaScript', ext: '.ts' },
  { id: 'python', name: 'Python', file: 'name-generator.py', category: 'Scripting', paradigm: 'High-level Multi-paradigm', ext: '.py' },
  { id: 'ruby', name: 'Ruby', file: 'name-generator.rb', category: 'Scripting', paradigm: 'Dynamic Object-Oriented', ext: '.rb' },
  { id: 'lua', name: 'Lua', file: 'name-generator.lua', category: 'Scripting', paradigm: 'Lightweight Embeddable', ext: '.lua' },
  { id: 'perl', name: 'Perl', file: 'name-generator.pl', category: 'Scripting', paradigm: 'Text Processing', ext: '.pl' },
  { id: 'php', name: 'PHP', file: 'name-generator.php', category: 'Scripting', paradigm: 'Web & CLI Scripting', ext: '.php' },
  { id: 'julia', name: 'Julia', file: 'name-generator.jl', category: 'Scripting', paradigm: 'High-performance Numerical', ext: '.jl' },
  { id: 'r', name: 'R', file: 'name-generator.r', category: 'Scripting', paradigm: 'Statistical Computing', ext: '.r' },
  { id: 'raku', name: 'Raku', file: 'name-generator.raku', category: 'Scripting', paradigm: 'Expressive Multi-paradigm', ext: '.raku' },
  { id: 'octave', name: 'GNU Octave', file: 'name-generator.m', category: 'Scripting', paradigm: 'Matrix / Scientific', ext: '.m' },
  { id: 'elisp', name: 'Emacs Lisp', file: 'name-generator.el', category: 'Scripting', paradigm: 'Lisp Dialect', ext: '.el' },
  { id: 'racket', name: 'Racket', file: 'name-generator.rkt', category: 'Scripting', paradigm: 'Scheme / Lisp Family', ext: '.rkt' },
  { id: 'dart', name: 'Dart', file: 'name-generator.dart', category: 'VM', paradigm: 'Client-optimized OO', ext: '.dart' },

  { id: 'java', name: 'Java', file: 'NameGenerator.java', category: 'VM', paradigm: 'JVM Object-Oriented', ext: '.java' },
  { id: 'kotlin', name: 'Kotlin', file: 'name-generator.kt', category: 'VM', paradigm: 'JVM Multiplatform', ext: '.kt' },
  { id: 'scala', name: 'Scala', file: 'NameGeneratorScala.scala', category: 'VM', paradigm: 'JVM Functional / OO', ext: '.scala' },
  { id: 'clojure', name: 'Clojure', file: 'name-generator.clj', category: 'VM', paradigm: 'JVM Lisp', ext: '.clj' },
  { id: 'erlang', name: 'Erlang', file: 'name_generator.erl', category: 'VM', paradigm: 'BEAM Actor Model', ext: '.erl' },
  { id: 'elixir', name: 'Elixir', file: 'name-generator.exs', category: 'VM', paradigm: 'BEAM Functional', ext: '.exs' },
  { id: 'gleam', name: 'Gleam', file: 'name_generator_gleam', category: 'VM', paradigm: 'Type-safe BEAM', ext: '.gleam' },

  { id: 'swift', name: 'Swift', file: 'name-generator.swift', category: 'WIP', paradigm: 'General Purpose', ext: '.swift' },
  { id: 'pony', name: 'Pony', file: 'name-generator.pony', category: 'WIP', paradigm: 'Actor-model / Type-safe', ext: '.pony' },
  { id: 'solidity', name: 'Solidity', file: 'name-generator.sol', category: 'WIP', paradigm: 'Contract-oriented', ext: '.sol' },
  { id: 'brainfuck', name: 'Brainfuck', file: 'name-generator.bfk', category: 'Esoteric', paradigm: 'Turing Tarpit', ext: '.bfk' },
];

// Enrich languages with line count and code snippet
const languages = LANGUAGES_META.map((lang) => {
  const filePath = path.resolve(ROOT_DIR, lang.file);
  let sloc = 0;
  let codeSample = '';
  if (fs.existsSync(filePath)) {
    const stat = fs.statSync(filePath);
    if (stat.isFile()) {
      const content = fs.readFileSync(filePath, 'utf-8');
      const lines = content.split('\n');
      sloc = lines.length;
      codeSample = lines.slice(0, 30).join('\n');
    }
  }
  return {
    ...lang,
    sloc,
    codeSample,
    githubUrl: `https://github.com/joshuacox/name-generator/blob/main/${lang.file}`,
  };
});

// Map CLI commands to friendly names
const COMMAND_TO_NAME = {
  './name-generator_zig': 'Zig',
  './name-generator_go': 'Go',
  './name-generator_crystal': 'Crystal',
  './name-generator_nim': 'Nim',
  './name-generator_odin': 'Odin',
  './name-generator_ada': 'Ada',
  './name-generator_cpp': 'C++',
  './name-generator_v': 'V',
  './name-generator_fortran': 'Fortran',
  './name-generator': 'C',
  'rust/target/debug/name-generator': 'Rust',
  'rust/target/release/name-generator': 'Rust',
  './name-generator_d': 'D',
  './name-generator_pascal': 'Pascal',

  './name-generator.awk': 'AWK',
  './name-generator.js': 'Node.js',
  './name-generator.rb': 'Ruby',
  './name-generator.tcl': 'Tcl',
  './name-generator.zsh': 'Zsh',
  './name-generator.sh': 'POSIX sh',
  './name-generator.bash': 'Bash',
  './name-generator.ksh': 'KornShell (ksh)',
  './name-generator.nu': 'Nushell',
  './name-generator.py': 'Python',
  './name-generator.pl': 'Perl',
  './name-generator.php': 'PHP',
  './name-generator.lua': 'Lua',
  './name-generator.ts': 'TypeScript',
};

// Default baseline benchmarks
let deathmatchCompiled = [
  { name: 'Zig', command: './name-generator_zig', mean: 1.2, min: 1.1, max: 1.2, stddev: 0.04, relative: 1.00, paradigm: 'Systems' },
  { name: 'Go', command: './name-generator_go', mean: 2.0, min: 2.0, max: 2.1, stddev: 0.05, relative: 1.74, paradigm: 'Concurrent' },
  { name: 'Crystal', command: './name-generator_crystal', mean: 2.6, min: 2.5, max: 2.6, stddev: 0.04, relative: 2.20, paradigm: 'Object-Oriented' },
  { name: 'Nim', command: './name-generator_nim', mean: 3.8, min: 3.8, max: 3.9, stddev: 0.05, relative: 3.29, paradigm: 'Multi-paradigm' },
  { name: 'Odin', command: './name-generator_odin', mean: 4.7, min: 4.7, max: 4.7, stddev: 0.03, relative: 4.05, paradigm: 'Systems' },
  { name: 'Ada', command: './name-generator_ada', mean: 7.3, min: 7.2, max: 7.4, stddev: 0.08, relative: 6.26, paradigm: 'Safety-critical' },
  { name: 'C++', command: './name-generator_cpp', mean: 10.6, min: 10.4, max: 10.8, stddev: 0.12, relative: 9.07, paradigm: 'Systems' },
  { name: 'V', command: './name-generator_v', mean: 37.0, min: 30.2, max: 68.3, stddev: 13.9, relative: 31.81, paradigm: 'Systems' },
  { name: 'Fortran', command: './name-generator_fortran', mean: 44.0, min: 11.8, max: 66.9, stddev: 25.1, relative: 37.83, paradigm: 'Scientific' },
];

let deathmatchScripting = [
  { name: 'AWK', command: './name-generator.awk', mean: 19.3, min: 18.7, max: 20.9, relative: 1.00, type: 'Scripting' },
  { name: 'Node.js', command: './name-generator.js', mean: 25.2, min: 23.0, max: 26.5, relative: 1.30, type: 'Runtime' },
  { name: 'Ruby', command: './name-generator.rb', mean: 42.9, min: 42.6, max: 43.2, relative: 2.22, type: 'Scripting' },
  { name: 'Tcl', command: './name-generator.tcl', mean: 56.6, min: 28.7, max: 66.8, relative: 2.93, type: 'Scripting' },
  { name: 'Zsh', command: './name-generator.zsh', mean: 156.6, min: 150.7, max: 170.5, relative: 8.11, type: 'Shell' },
  { name: 'POSIX sh', command: './name-generator.sh', mean: 178.9, min: 171.7, max: 188.2, relative: 9.26, type: 'Shell' },
  { name: 'Bash', command: './name-generator.bash', mean: 203.3, min: 156.6, max: 268.9, relative: 10.53, type: 'Shell' },
  { name: 'KornShell (ksh)', command: './name-generator.ksh', mean: 285.3, min: 110.4, max: 516.3, relative: 14.77, type: 'Shell' },
  { name: 'Nushell', command: './name-generator.nu', mean: 322.0, min: 251.6, max: 380.3, relative: 16.67, type: 'Shell' },
];

// Ingest live CI JSON outputs if present
const ciCompiledFile = path.join(LOG_DIR, 'ci-compiled.json');
if (fs.existsSync(ciCompiledFile)) {
  try {
    const raw = JSON.parse(fs.readFileSync(ciCompiledFile, 'utf-8'));
    if (raw.results && raw.results.length > 0) {
      const minMean = Math.min(...raw.results.map((r) => r.mean));
      deathmatchCompiled = raw.results.map((r) => {
        const cmd = r.command;
        const name = COMMAND_TO_NAME[cmd] || path.basename(cmd);
        const meanMs = r.mean * 1000;
        const minMs = r.min * 1000;
        const maxMs = r.max * 1000;
        const stddevMs = r.stddev * 1000;
        return {
          name,
          command: cmd,
          mean: Number(meanMs.toFixed(2)),
          min: Number(minMs.toFixed(2)),
          max: Number(maxMs.toFixed(2)),
          stddev: Number(stddevMs.toFixed(2)),
          relative: Number((r.mean / minMean).toFixed(2)),
          paradigm: 'Systems',
        };
      }).sort((a, b) => a.mean - b.mean);
      console.log(`[CI] Ingested ${deathmatchCompiled.length} fresh compiled benchmark results from ${ciCompiledFile}`);
    }
  } catch (err) {
    console.warn(`Could not parse ${ciCompiledFile}:`, err);
  }
}

const ciScriptingFile = path.join(LOG_DIR, 'ci-scripting.json');
if (fs.existsSync(ciScriptingFile)) {
  try {
    const raw = JSON.parse(fs.readFileSync(ciScriptingFile, 'utf-8'));
    if (raw.results && raw.results.length > 0) {
      const minMean = Math.min(...raw.results.map((r) => r.mean));
      deathmatchScripting = raw.results.map((r) => {
        const cmd = r.command;
        const name = COMMAND_TO_NAME[cmd] || path.basename(cmd);
        const meanMs = r.mean * 1000;
        const minMs = r.min * 1000;
        const maxMs = r.max * 1000;
        return {
          name,
          command: cmd,
          mean: Number(meanMs.toFixed(1)),
          min: Number(minMs.toFixed(1)),
          max: Number(maxMs.toFixed(1)),
          relative: Number((r.mean / minMean).toFixed(2)),
          type: cmd.includes('.sh') || cmd.includes('.bash') || cmd.includes('.zsh') || cmd.includes('.ksh') || cmd.includes('.nu') ? 'Shell' : 'Scripting',
        };
      }).sort((a, b) => a.mean - b.mean);
      console.log(`[CI] Ingested ${deathmatchScripting.length} fresh scripting benchmark results from ${ciScriptingFile}`);
    }
  } catch (err) {
    console.warn(`Could not parse ${ciScriptingFile}:`, err);
  }
}

// Ingest Multi-Scale Scaling Benchmarks (N=1, 10, 100, 1000)
const scalingFile = path.join(LOG_DIR, 'scaling-benchmarks.json');
let scalingCurves = [];
let throughputLeaderboard = [];

if (fs.existsSync(scalingFile)) {
  try {
    const rawScaling = JSON.parse(fs.readFileSync(scalingFile, 'utf-8'));
    const map = {};
    rawScaling.results.forEach((r) => {
      const cmd = r.command.replace(/^counto=\d+\s+/, '');
      const count = Number(r.parameters.counto);
      const meanMs = Number((r.mean * 1000).toFixed(2));
      if (!map[cmd]) map[cmd] = [];
      map[cmd].push({ count, meanMs });
    });

    Object.entries(map).forEach(([cmd, points]) => {
      points.sort((a, b) => a.count - b.count);
      const name = COMMAND_TO_NAME[cmd] || path.basename(cmd);
      const category = cmd.includes('.sh') || cmd.includes('.bash') ? 'Shell' : cmd.includes('.') ? 'Scripting' : 'Compiled';
      scalingCurves.push({
        language: name,
        command: cmd,
        category,
        points,
      });

      // Calculate throughput at highest count (usually 1000)
      const pt1000 = points.find((p) => p.count === 1000);
      if (pt1000 && pt1000.meanMs > 0) {
        const namesPerSecond = Math.round((1000 / pt1000.meanMs) * 1000);
        throughputLeaderboard.push({
          name,
          category,
          namesPerSecond,
          meanMsAt1000: pt1000.meanMs,
          relativeToFastest: 1, // updated below
        });
      }
    });

    // Sort throughput
    throughputLeaderboard.sort((a, b) => b.namesPerSecond - a.namesPerSecond);
    const maxThroughput = throughputLeaderboard[0]?.namesPerSecond || 1;
    throughputLeaderboard.forEach((item) => {
      item.relativeToFastest = Number((maxThroughput / Math.max(1, item.namesPerSecond)).toFixed(2));
    });

    console.log(`[CI] Ingested ${scalingCurves.length} scaling curves and throughput metrics from ${scalingFile}`);
  } catch (err) {
    console.warn(`Could not parse ${scalingFile}:`, err);
  }
}

// Fallback baseline scaling curves if not yet present
if (scalingCurves.length === 0) {
  scalingCurves = [
    { language: 'Zig', command: './name-generator_zig', category: 'Compiled', points: [{ count: 1, meanMs: 1.11 }, { count: 10, meanMs: 1.12 }, { count: 100, meanMs: 1.12 }, { count: 1000, meanMs: 1.20 }] },
    { language: 'Go', command: './name-generator_go', category: 'Compiled', points: [{ count: 1, meanMs: 1.71 }, { count: 10, meanMs: 1.69 }, { count: 100, meanMs: 1.73 }, { count: 1000, meanMs: 2.19 }] },
    { language: 'Crystal', command: './name-generator_crystal', category: 'Compiled', points: [{ count: 1, meanMs: 2.31 }, { count: 10, meanMs: 2.26 }, { count: 100, meanMs: 2.33 }, { count: 1000, meanMs: 2.52 }] },
    { language: 'Nim', command: './name-generator_nim', category: 'Compiled', points: [{ count: 1, meanMs: 3.58 }, { count: 10, meanMs: 3.61 }, { count: 100, meanMs: 3.65 }, { count: 1000, meanMs: 3.75 }] },
    { language: 'Odin', command: './name-generator_odin', category: 'Compiled', points: [{ count: 1, meanMs: 4.33 }, { count: 10, meanMs: 4.37 }, { count: 100, meanMs: 4.30 }, { count: 1000, meanMs: 4.60 }] },
    { language: 'Ada', command: './name-generator_ada', category: 'Compiled', points: [{ count: 1, meanMs: 6.94 }, { count: 10, meanMs: 7.26 }, { count: 100, meanMs: 7.03 }, { count: 1000, meanMs: 12.48 }] },
    { language: 'AWK', command: './name-generator.awk', category: 'Scripting', points: [{ count: 1, meanMs: 23.75 }, { count: 10, meanMs: 29.18 }, { count: 100, meanMs: 28.56 }, { count: 1000, meanMs: 40.17 }] },
    { language: 'Node.js', command: './name-generator.js', category: 'Scripting', points: [{ count: 1, meanMs: 71.08 }, { count: 10, meanMs: 57.63 }, { count: 100, meanMs: 28.97 }, { count: 1000, meanMs: 72.13 }] },
    { language: 'Bash', command: './name-generator.bash', category: 'Shell', points: [{ count: 1, meanMs: 5.40 }, { count: 10, meanMs: 20.02 }, { count: 100, meanMs: 269.17 }, { count: 1000, meanMs: 2464.61 }] },
  ];

  throughputLeaderboard = [
    { name: 'Zig', category: 'Compiled', namesPerSecond: 833333, meanMsAt1000: 1.20, relativeToFastest: 1.00 },
    { name: 'Go', category: 'Compiled', namesPerSecond: 456621, meanMsAt1000: 2.19, relativeToFastest: 1.82 },
    { name: 'Crystal', category: 'Compiled', namesPerSecond: 396825, meanMsAt1000: 2.52, relativeToFastest: 2.10 },
    { name: 'Nim', category: 'Compiled', namesPerSecond: 266666, meanMsAt1000: 3.75, relativeToFastest: 3.12 },
    { name: 'Odin', category: 'Compiled', namesPerSecond: 217391, meanMsAt1000: 4.60, relativeToFastest: 3.83 },
    { name: 'Ada', category: 'Compiled', namesPerSecond: 80128, meanMsAt1000: 12.48, relativeToFastest: 10.40 },
    { name: 'AWK', category: 'Scripting', namesPerSecond: 24894, meanMsAt1000: 40.17, relativeToFastest: 33.48 },
    { name: 'Node.js', category: 'Scripting', namesPerSecond: 13863, meanMsAt1000: 72.13, relativeToFastest: 60.11 },
    { name: 'Bash', category: 'Shell', namesPerSecond: 405, meanMsAt1000: 2464.61, relativeToFastest: 2057.61 },
  ];
}

// Parser for scanner CSVs
function parseCsv(filename) {
  const filePath = path.resolve(ROOT_DIR, 'docs', filename);
  if (!fs.existsSync(filePath)) return [];
  const content = fs.readFileSync(filePath, 'utf-8');
  const lines = content.trim().split('\n');
  if (lines.length < 2) return [];

  const headers = lines[0].split(',');
  const results = [];

  for (let i = 1; i < lines.length; i++) {
    const parts = lines[i].split(',');
    if (parts.length < headers.length) continue;
    const row = {};
    headers.forEach((h, idx) => {
      const val = parts[idx];
      row[h.trim()] = isNaN(Number(val)) ? val : Number(val);
    });
    const cmd = String(row.command || '');
    const cleanCmd = cmd.replace(/^counto=\$\(\(2\*\*\{?num_count\}?\)\)\s+/, '').replace(/^\.\//, '');
    row.cleanCmd = cleanCmd;
    results.push(row);
  }
  return results;
}

const scannerDatasets = {
  fastest_24: parseCsv('fastest_scanner-24.csv'),
  faster_20: parseCsv('faster_scanner-20.csv'),
  fast_15: parseCsv('fast_scanner-15.csv'),
  scanner_11: parseCsv('scanner-11.csv'),
  slow_5: parseCsv('slow_scanner-5.csv'),
  slowest_10: parseCsv('slowest_scanner-10.csv'),
};

const fastestCompiled = deathmatchCompiled[0] || { name: 'Zig', mean: 1.2 };
const fastestScript = deathmatchScripting[0] || { name: 'AWK', mean: 19.3 };

const fullBenchmarkData = {
  generatedAt: new Date().toISOString(),
  environment: {
    os: 'Linux (Ubuntu x86_64)',
    cpu: 'Host Virtualized Multi-core',
    tool: 'hyperfine',
  },
  stats: {
    totalLanguages: languages.length,
    compiledContenders: deathmatchCompiled.length,
    scriptingContenders: deathmatchScripting.length,
    fastestLanguage: fastestCompiled.name,
    fastestMeanMs: fastestCompiled.mean,
    fastestScripting: fastestScript.name,
    fastestScriptingMs: fastestScript.mean,
    maxThroughputPerSec: throughputLeaderboard[0]?.namesPerSecond || 833333,
  },
  deathmatchCompiled,
  deathmatchScripting,
  scalingCurves,
  throughputLeaderboard,
  languages,
  scanners: {
    fastest_24_summary: scannerDatasets.fastest_24.slice(0, 100),
  },
};

const jsonStr = JSON.stringify(fullBenchmarkData, null, 2);

fs.writeFileSync(path.join(WEB_DATA_DIR, 'benchmarks.json'), jsonStr);
fs.writeFileSync(path.join(WEB_PUBLIC_DATA_DIR, 'benchmarks.json'), jsonStr);

console.log(`Successfully generated benchmark datasets:`);
console.log(`- Total registered languages: ${languages.length}`);
console.log(`- Scaling curves: ${scalingCurves.length}`);
console.log(`- Throughput items: ${throughputLeaderboard.length}`);
console.log(`- Saved to ${path.join(WEB_DATA_DIR, 'benchmarks.json')}`);
console.log(`- Saved to ${path.join(WEB_PUBLIC_DATA_DIR, 'benchmarks.json')}`);
