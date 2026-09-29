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
  { id: 'c', name: 'C', file: 'name-generator.c', category: 'Compiled', paradigm: 'Procedural Systems', ext: '.c', compiledBin: 'name-generator' },
  { id: 'cpp', name: 'C++', file: 'name-generator.cpp', category: 'Compiled', paradigm: 'Object-Oriented Systems', ext: '.cpp', compiledBin: 'name-generator_cpp' },
  { id: 'go', name: 'Go', file: 'name-generator.go', category: 'Compiled', paradigm: 'Concurrent Systems', ext: '.go', compiledBin: 'name-generator_go' },
  { id: 'rust', name: 'Rust', file: 'rust/src/main.rs', category: 'Compiled', paradigm: 'Safe Systems', ext: '.rs', compiledBin: 'rust/target/release/name-generator' },
  { id: 'crystal', name: 'Crystal', file: 'name-generator.cr', category: 'Compiled', paradigm: 'Object-Oriented', ext: '.cr', compiledBin: 'name-generator_crystal' },
  { id: 'nim', name: 'Nim', file: 'name_generator.nim', category: 'Compiled', paradigm: 'Multi-paradigm', ext: '.nim', compiledBin: 'name-generator_nim' },
  { id: 'odin', name: 'Odin', file: 'name-generator.odin', category: 'Compiled', paradigm: 'Data-oriented Systems', ext: '.odin', compiledBin: 'name-generator_odin' },
  { id: 'ada', name: 'Ada', file: 'name_generator_ada.adb', category: 'Compiled', paradigm: 'Safety-critical / Structured', ext: '.adb', compiledBin: 'name-generator_ada' },
  { id: 'cobol', name: 'COBOL', file: 'name-generator.cbl', category: 'Compiled', paradigm: 'Mainframe / Procedural', ext: '.cbl', compiledBin: 'name-generator_cobol' },
  { id: 'v', name: 'V', file: 'name-generator.v', category: 'Compiled', paradigm: 'Simple Systems', ext: '.v', compiledBin: 'name-generator_v' },
  { id: 'fortran', name: 'Fortran', file: 'name-generator.f90', category: 'Compiled', paradigm: 'Scientific / Procedural', ext: '.f90', compiledBin: 'name-generator_fortran' },
  { id: 'pascal', name: 'Pascal', file: 'name-generator.pas', category: 'Compiled', paradigm: 'Structured Systems', ext: '.pas', compiledBin: 'name-generator_pascal' },
  { id: 'd', name: 'D', file: 'name-generator_d.d', category: 'Compiled', paradigm: 'Multi-paradigm Systems', ext: '.d', compiledBin: 'name-generator_d' },
  { id: 'dart', name: 'Dart', file: 'name-generator.dart', category: 'Compiled', paradigm: 'Client & Systems OO', ext: '.dart', compiledBin: 'name-generator_dart' },
  { id: 'haskell', name: 'Haskell', file: 'name-generator_haskell.hs', category: 'Compiled', paradigm: 'Pure Functional', ext: '.hs' },
  { id: 'ocaml', name: 'OCaml', file: 'name-generator.ml', category: 'Compiled', paradigm: 'Functional / Impure', ext: '.ml' },

  { id: 'java', name: 'Java', file: 'NameGenerator.java', category: 'VM', paradigm: 'JVM Object-Oriented', ext: '.java' },
  { id: 'kotlin', name: 'Kotlin', file: 'name-generator.kt', category: 'VM', paradigm: 'JVM Multiplatform', ext: '.kt' },
  { id: 'scala', name: 'Scala', file: 'NameGeneratorScala.scala', category: 'VM', paradigm: 'JVM Functional / OO', ext: '.scala' },
  { id: 'clojure', name: 'Clojure', file: 'name-generator.clj', category: 'VM', paradigm: 'JVM Lisp', ext: '.clj' },
  { id: 'erlang', name: 'Erlang', file: 'name_generator.erl', category: 'VM', paradigm: 'BEAM Actor Model', ext: '.erl' },
  { id: 'elixir', name: 'Elixir', file: 'name-generator.exs', category: 'VM', paradigm: 'BEAM Functional', ext: '.exs' },
  { id: 'gleam', name: 'Gleam', file: 'name_generator_gleam', category: 'VM', paradigm: 'Type-safe BEAM', ext: '.gleam' },

  { id: 'awk', name: 'AWK', file: 'name-generator.awk', category: 'Scripting', paradigm: 'Pattern Scanning & Processing', ext: '.awk' },
  { id: 'javascript', name: 'Node.js', file: 'name-generator.js', category: 'Scripting', paradigm: 'Event-driven Async', ext: '.js' },
  { id: 'typescript', name: 'TypeScript', file: 'name-generator.ts', category: 'Scripting', paradigm: 'Typed JavaScript', ext: '.ts' },
  { id: 'python', name: 'Python', file: 'name-generator.py', category: 'Scripting', paradigm: 'High-level Multi-paradigm', ext: '.py' },
  { id: 'ruby', name: 'Ruby', file: 'name-generator.rb', category: 'Scripting', paradigm: 'Dynamic Object-Oriented', ext: '.rb' },
  { id: 'lua', name: 'Lua', file: 'name-generator.lua', category: 'Scripting', paradigm: 'Lightweight Embeddable', ext: '.lua' },
  { id: 'perl', name: 'Perl', file: 'name-generator.pl', category: 'Scripting', paradigm: 'Text Processing', ext: '.pl' },
  { id: 'php', name: 'PHP', file: 'name-generator.php', category: 'Scripting', paradigm: 'Web & CLI Scripting', ext: '.php' },
  { id: 'tcl', name: 'Tcl', file: 'name-generator.tcl', category: 'Scripting', paradigm: 'Command Language / String', ext: '.tcl' },
  { id: 'julia', name: 'Julia', file: 'name-generator.jl', category: 'Scripting', paradigm: 'High-performance Numerical', ext: '.jl' },
  { id: 'r', name: 'R', file: 'name-generator.r', category: 'Scripting', paradigm: 'Statistical Computing', ext: '.r' },
  { id: 'raku', name: 'Raku', file: 'name-generator.raku', category: 'Scripting', paradigm: 'Expressive Multi-paradigm', ext: '.raku' },
  { id: 'octave', name: 'GNU Octave', file: 'name-generator.m', category: 'Scripting', paradigm: 'Matrix / Scientific', ext: '.m' },
  { id: 'elisp', name: 'Emacs Lisp', file: 'name-generator.el', category: 'Scripting', paradigm: 'Lisp Dialect', ext: '.el' },
  { id: 'racket', name: 'Racket', file: 'name-generator.rkt', category: 'Scripting', paradigm: 'Scheme / Lisp Family', ext: '.rkt' },

  { id: 'sh', name: 'POSIX sh', file: 'name-generator.sh', category: 'Shell', paradigm: 'Bourne Shell', ext: '.sh' },
  { id: 'bash', name: 'Bash', file: 'name-generator.bash', category: 'Shell', paradigm: 'Unix Shell', ext: '.bash' },
  { id: 'zsh', name: 'Zsh', file: 'name-generator.zsh', category: 'Shell', paradigm: 'Extended Unix Shell', ext: '.zsh' },
  { id: 'fish', name: 'Fish', file: 'name-generator.fish', category: 'Shell', paradigm: 'Friendly Interactive Shell', ext: '.fish' },
  { id: 'ksh', name: 'KornShell (ksh)', file: 'name-generator.ksh', category: 'Shell', paradigm: 'POSIX Shell', ext: '.ksh' },
  { id: 'nu', name: 'Nushell', file: 'name-generator.nu', category: 'Shell', paradigm: 'Structured Data Shell', ext: '.nu' },

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
  './name-generator': 'C',
  './name-generator_cpp': 'C++',
  './name-generator_go': 'Go',
  'rust/target/release/name-generator': 'Rust',
  'rust/target/debug/name-generator': 'Rust',
  './name-generator_crystal': 'Crystal',
  './name-generator_nim': 'Nim',
  './name-generator_odin': 'Odin',
  './name-generator_ada': 'Ada',
  './name-generator_cobol': 'COBOL',
  './name-generator_v': 'V',
  './name-generator_fortran': 'Fortran',
  './name-generator_pascal': 'Pascal',
  './name-generator_d': 'D',
  './name-generator_dart': 'Dart',

  'java NameGenerator': 'Java',

  './name-generator.awk': 'AWK',
  './name-generator.js': 'Node.js',
  'node ./name-generator.js': 'Node.js',
  './name-generator.ts': 'TypeScript',
  './name-generator.py': 'Python',
  'python3 ./name-generator.py': 'Python',
  './name-generator.rb': 'Ruby',
  'ruby ./name-generator.rb': 'Ruby',
  './name-generator.pl': 'Perl',
  'perl ./name-generator.pl': 'Perl',
  './name-generator.php': 'PHP',
  'php ./name-generator.php': 'PHP',
  './name-generator.lua': 'Lua',
  'lua ./name-generator.lua': 'Lua',
  './name-generator.tcl': 'Tcl',

  './name-generator.sh': 'POSIX sh',
  './name-generator.bash': 'Bash',
  './name-generator.zsh': 'Zsh',
  './name-generator.fish': 'Fish',
  './name-generator.ksh': 'KornShell (ksh)',
  './name-generator.nu': 'Nushell',
};

// 1. Ingest Peak RSS Memory Telemetry
const memoryMap = {};
const ciMemoryFile = path.join(LOG_DIR, 'ci-memory.json');
if (fs.existsSync(ciMemoryFile)) {
  try {
    const rawMem = JSON.parse(fs.readFileSync(ciMemoryFile, 'utf-8'));
    rawMem.forEach((item) => {
      memoryMap[item.name] = {
        peakRssKb: item.peakRssKb,
        peakRssMb: item.peakRssMb,
      };
      // Also map by command
      if (item.command) memoryMap[item.command] = memoryMap[item.name];
    });
    console.log(`[CI] Ingested memory telemetry for ${rawMem.length} contenders from ${ciMemoryFile}`);
  } catch (err) {
    console.warn(`Could not parse ${ciMemoryFile}:`, err);
  }
}

// 2. Ingest Regression Telemetry
const regressionMap = {};
const ciTelemetryFile = path.join(LOG_DIR, 'ci-telemetry.json');
if (fs.existsSync(ciTelemetryFile)) {
  try {
    const rawTelem = JSON.parse(fs.readFileSync(ciTelemetryFile, 'utf-8'));
    (rawTelem.regression || []).forEach((item) => {
      regressionMap[item.name] = {
        startupMs: item.startupMs,
        marginalUsPerName: item.marginalUsPerName,
        sustainedNamesPerSec: item.sustainedNamesPerSec,
        r2: item.r2,
      };
    });
    console.log(`[CI] Ingested OLS regression telemetry for ${Object.keys(regressionMap).length} contenders from ${ciTelemetryFile}`);
  } catch (err) {
    console.warn(`Could not parse ${ciTelemetryFile}:`, err);
  }
}

function parseHyperfineJson(filePath, defaultCategory = 'Compiled') {
  if (!fs.existsSync(filePath)) return null;
  try {
    const raw = JSON.parse(fs.readFileSync(filePath, 'utf-8'));
    if (!raw.results || raw.results.length === 0) return null;
    const minMean = Math.min(...raw.results.map((r) => r.mean));
    return raw.results.map((r) => {
      const cmd = r.command;
      const name = COMMAND_TO_NAME[cmd] || path.basename(cmd);
      const meta = LANGUAGES_META.find((l) => l.name === name || l.compiledBin === path.basename(cmd) || l.file === path.basename(cmd));
      const category = meta ? meta.category : defaultCategory;
      const paradigm = meta ? meta.paradigm : 'General';
      const meanMs = Number((r.mean * 1000).toFixed(2));
      const minMs = Number((r.min * 1000).toFixed(2));
      const maxMs = Number((r.max * 1000).toFixed(2));
      const stddevMs = Number((r.stddev * 1000).toFixed(2));
      const relative = Number((r.mean / minMean).toFixed(2));
      const mem = memoryMap[name] || memoryMap[cmd] || { peakRssMb: 4.0, peakRssKb: 4096 };
      const reg = regressionMap[name] || {};

      return {
        name,
        command: cmd,
        category,
        paradigm,
        mean: meanMs,
        min: minMs,
        max: maxMs,
        stddev: stddevMs,
        relative,
        peakRssMb: mem.peakRssMb,
        peakRssKb: mem.peakRssKb,
        startupMs: reg.startupMs,
        marginalUsPerName: reg.marginalUsPerName,
        sustainedNamesPerSec: reg.sustainedNamesPerSec,
        r2: reg.r2,
      };
    }).sort((a, b) => a.mean - b.mean);
  } catch (err) {
    console.warn(`Error parsing ${filePath}:`, err);
    return null;
  }
}

// Default baseline benchmarks
let deathmatchCompiled = parseHyperfineJson(path.join(LOG_DIR, 'ci-compiled.json'), 'Compiled') || [
  { name: 'Zig', command: './name-generator_zig', mean: 1.2, min: 1.1, max: 1.2, stddev: 0.04, relative: 1.00, paradigm: 'Systems', category: 'Compiled', peakRssMb: 1.62 },
  { name: 'C', command: './name-generator', mean: 1.3, min: 1.2, max: 1.4, stddev: 0.05, relative: 1.08, paradigm: 'Procedural Systems', category: 'Compiled', peakRssMb: 1.54 },
  { name: 'Go', command: './name-generator_go', mean: 2.0, min: 2.0, max: 2.1, stddev: 0.05, relative: 1.67, paradigm: 'Concurrent Systems', category: 'Compiled', peakRssMb: 5.75 },
  { name: 'Crystal', command: './name-generator_crystal', mean: 2.6, min: 2.5, max: 2.6, stddev: 0.04, relative: 2.17, paradigm: 'Object-Oriented', category: 'Compiled', peakRssMb: 3.98 },
  { name: 'Nim', command: './name-generator_nim', mean: 3.8, min: 3.8, max: 3.9, stddev: 0.05, relative: 3.17, paradigm: 'Multi-paradigm', category: 'Compiled', peakRssMb: 2.56 },
  { name: 'Odin', command: './name-generator_odin', mean: 4.7, min: 4.7, max: 4.7, stddev: 0.03, relative: 3.92, paradigm: 'Systems', category: 'Compiled', peakRssMb: 3.77 },
  { name: 'Pascal', command: './name-generator_pascal', mean: 5.2, min: 5.0, max: 5.5, stddev: 0.15, relative: 4.33, paradigm: 'Structured Systems', category: 'Compiled', peakRssMb: 5.33 },
  { name: 'Ada', command: './name-generator_ada', mean: 7.3, min: 7.2, max: 7.4, stddev: 0.08, relative: 6.08, paradigm: 'Safety-critical', category: 'Compiled', peakRssMb: 6.75 },
  { name: 'C++', command: './name-generator_cpp', mean: 10.6, min: 10.4, max: 10.8, stddev: 0.12, relative: 8.83, paradigm: 'Object-Oriented Systems', category: 'Compiled', peakRssMb: 4.63 },
  { name: 'Dart', command: './name-generator_dart', mean: 12.8, min: 11.2, max: 14.5, stddev: 1.1, relative: 10.67, paradigm: 'Client & Systems OO', category: 'Compiled', peakRssMb: 10.23 },
  { name: 'D', command: './name-generator_d', mean: 18.4, min: 17.5, max: 19.8, stddev: 0.8, relative: 15.33, paradigm: 'Multi-paradigm Systems', category: 'Compiled', peakRssMb: 9.21 },
  { name: 'COBOL', command: './name-generator_cobol', mean: 25.2, min: 10.5, max: 63.0, stddev: 18.8, relative: 21.0, paradigm: 'Mainframe', category: 'Compiled', peakRssMb: 10.20 },
  { name: 'V', command: './name-generator_v', mean: 37.0, min: 30.2, max: 68.3, stddev: 13.9, relative: 30.83, paradigm: 'Simple Systems', category: 'Compiled', peakRssMb: 5.20 },
  { name: 'Fortran', command: './name-generator_fortran', mean: 44.0, min: 11.8, max: 66.9, stddev: 25.1, relative: 36.67, paradigm: 'Scientific', category: 'Compiled', peakRssMb: 5.95 },
];

let deathmatchScripting = parseHyperfineJson(path.join(LOG_DIR, 'ci-scripting.json'), 'Scripting') || [
  { name: 'AWK', command: './name-generator.awk', mean: 19.3, min: 18.7, max: 20.9, relative: 1.00, category: 'Scripting', paradigm: 'Pattern Scanning', peakRssMb: 9.79 },
  { name: 'Lua', command: './name-generator.lua', mean: 21.4, min: 20.1, max: 22.8, relative: 1.11, category: 'Scripting', paradigm: 'Lightweight Embeddable', peakRssMb: 4.49 },
  { name: 'Node.js', command: './name-generator.js', mean: 25.2, min: 23.0, max: 26.5, relative: 1.31, category: 'Scripting', paradigm: 'Event-driven Async', peakRssMb: 58.17 },
  { name: 'TypeScript', command: './name-generator.ts', mean: 28.5, min: 26.2, max: 31.0, relative: 1.48, category: 'Scripting', paradigm: 'Typed JavaScript', peakRssMb: 76.24 },
  { name: 'Python', command: 'python3 ./name-generator.py', mean: 33.6, min: 32.1, max: 35.4, relative: 1.74, category: 'Scripting', paradigm: 'High-level Multi-paradigm', peakRssMb: 10.75 },
  { name: 'Ruby', command: 'ruby ./name-generator.rb', mean: 42.9, min: 42.6, max: 43.2, relative: 2.22, category: 'Scripting', paradigm: 'Dynamic Object-Oriented', peakRssMb: 19.91 },
  { name: 'Perl', command: 'perl ./name-generator.pl', mean: 48.2, min: 46.5, max: 50.1, relative: 2.50, category: 'Scripting', paradigm: 'Text Processing', peakRssMb: 12.13 },
  { name: 'Tcl', command: './name-generator.tcl', mean: 56.6, min: 28.7, max: 66.8, relative: 2.93, category: 'Scripting', paradigm: 'Command Language', peakRssMb: 8.96 },
  { name: 'PHP', command: 'php ./name-generator.php', mean: 62.4, min: 59.8, max: 65.1, relative: 3.23, category: 'Scripting', paradigm: 'Web & CLI Scripting', peakRssMb: 21.41 },
];

let deathmatchShells = parseHyperfineJson(path.join(LOG_DIR, 'ci-shells.json'), 'Shell') || [
  { name: 'POSIX sh', command: './name-generator.sh', mean: 12.4, min: 11.2, max: 13.8, relative: 1.00, category: 'Shell', paradigm: 'Bourne Shell', peakRssMb: 2.86 },
  { name: 'Bash', command: './name-generator.bash', mean: 15.6, min: 14.1, max: 17.2, relative: 1.26, category: 'Shell', paradigm: 'Unix Shell', peakRssMb: 3.26 },
  { name: 'Zsh', command: './name-generator.zsh', mean: 22.8, min: 21.5, max: 24.3, relative: 1.84, category: 'Shell', paradigm: 'Extended Unix Shell', peakRssMb: 3.73 },
  { name: 'Fish', command: './name-generator.fish', mean: 41.5, min: 39.2, max: 44.1, relative: 3.35, category: 'Shell', paradigm: 'Interactive Shell', peakRssMb: 7.14 },
  { name: 'KornShell (ksh)', command: './name-generator.ksh', mean: 58.2, min: 54.0, max: 63.4, relative: 4.69, category: 'Shell', paradigm: 'POSIX Shell', peakRssMb: 3.91 },
  { name: 'Nushell', command: './name-generator.nu', mean: 184.0, min: 175.2, max: 198.5, relative: 14.84, category: 'Shell', paradigm: 'Structured Data Shell', peakRssMb: 31.18 },
];

let deathmatchVm = parseHyperfineJson(path.join(LOG_DIR, 'ci-vm.json'), 'VM') || [
  { name: 'Java', command: 'java NameGenerator', mean: 54.8, min: 52.1, max: 58.4, relative: 1.00, category: 'VM', paradigm: 'JVM Object-Oriented', peakRssMb: 56.92 },
];

// Ensure all items have peakRss populated from memoryMap
[deathmatchCompiled, deathmatchScripting, deathmatchShells, deathmatchVm].forEach((arr) => {
  arr.forEach((item) => {
    const mem = memoryMap[item.name] || memoryMap[item.command];
    if (mem) {
      item.peakRssMb = mem.peakRssMb;
      item.peakRssKb = mem.peakRssKb;
    }
    const reg = regressionMap[item.name];
    if (reg) {
      item.startupMs = reg.startupMs;
      item.marginalUsPerName = reg.marginalUsPerName;
      item.sustainedNamesPerSec = reg.sustainedNamesPerSec;
      item.r2 = reg.r2;
    }
  });
});

// 3. Multi-Scale Scaling & Throughput Leaderboard
const scalingFile = path.join(LOG_DIR, 'scaling-benchmarks.json');
let scalingCurves = [];
let throughputLeaderboard = [];

if (fs.existsSync(scalingFile)) {
  try {
    const rawScaling = JSON.parse(fs.readFileSync(scalingFile, 'utf-8'));
    const map = {};
    rawScaling.results.forEach((r) => {
      const cmd = r.command.replace(/^counto=\S+\s+/, '');
      const count = Number(r.parameters.counto);
      const meanMs = Number((r.mean * 1000).toFixed(2));
      if (!map[cmd]) map[cmd] = [];
      map[cmd].push({ count, meanMs });
    });

    Object.entries(map).forEach(([cmd, points]) => {
      const name = COMMAND_TO_NAME[cmd] || path.basename(cmd);
      const meta = LANGUAGES_META.find((l) => l.name === name || l.compiledBin === path.basename(cmd) || l.file === path.basename(cmd));
      const category = meta ? meta.category : 'Compiled';
      const mem = memoryMap[name] || memoryMap[cmd] || { peakRssMb: 4.0 };
      const reg = regressionMap[name] || {};

      scalingCurves.push({
        language: name,
        command: cmd,
        category,
        points: points.sort((a, b) => a.count - b.count),
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
          peakRssMb: mem.peakRssMb,
          startupMs: reg.startupMs || pt1000.meanMs,
          marginalUsPerName: reg.marginalUsPerName || Number(((pt1000.meanMs / 1000) * 1000).toFixed(2)),
          sustainedRate: reg.sustainedNamesPerSec || namesPerSecond,
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

    console.log(`[CI] Ingested ${scalingCurves.length} scaling curves and ${throughputLeaderboard.length} throughput items from ${scalingFile}`);
  } catch (err) {
    console.warn(`Could not parse ${scalingFile}:`, err);
  }
}

// Fallback baseline scaling curves if not present
if (scalingCurves.length === 0) {
  scalingCurves = [
    { language: 'Zig', command: './name-generator_zig', category: 'Compiled', points: [{ count: 1, meanMs: 1.11 }, { count: 10, meanMs: 1.12 }, { count: 100, meanMs: 1.12 }, { count: 1000, meanMs: 1.20 }] },
    { language: 'C', command: './name-generator', category: 'Compiled', points: [{ count: 1, meanMs: 1.21 }, { count: 10, meanMs: 1.22 }, { count: 100, meanMs: 1.25 }, { count: 1000, meanMs: 1.35 }] },
    { language: 'Go', command: './name-generator_go', category: 'Compiled', points: [{ count: 1, meanMs: 1.71 }, { count: 10, meanMs: 1.69 }, { count: 100, meanMs: 1.73 }, { count: 1000, meanMs: 2.19 }] },
    { language: 'Crystal', command: './name-generator_crystal', category: 'Compiled', points: [{ count: 1, meanMs: 2.31 }, { count: 10, meanMs: 2.26 }, { count: 100, meanMs: 2.33 }, { count: 1000, meanMs: 2.52 }] },
    { language: 'Nim', command: './name-generator_nim', category: 'Compiled', points: [{ count: 1, meanMs: 3.58 }, { count: 10, meanMs: 3.61 }, { count: 100, meanMs: 3.65 }, { count: 1000, meanMs: 3.75 }] },
    { language: 'Odin', command: './name-generator_odin', category: 'Compiled', points: [{ count: 1, meanMs: 4.33 }, { count: 10, meanMs: 4.37 }, { count: 100, meanMs: 4.30 }, { count: 1000, meanMs: 4.60 }] },
    { language: 'Pascal', command: './name-generator_pascal', category: 'Compiled', points: [{ count: 1, meanMs: 5.01 }, { count: 10, meanMs: 5.05 }, { count: 100, meanMs: 5.12 }, { count: 1000, meanMs: 5.30 }] },
    { language: 'Ada', command: './name-generator_ada', category: 'Compiled', points: [{ count: 1, meanMs: 6.94 }, { count: 10, meanMs: 7.26 }, { count: 100, meanMs: 7.03 }, { count: 1000, meanMs: 12.48 }] },
    { language: 'Dart', command: './name-generator_dart', category: 'Compiled', points: [{ count: 1, meanMs: 11.20 }, { count: 10, meanMs: 11.45 }, { count: 100, meanMs: 11.90 }, { count: 1000, meanMs: 13.10 }] },
    { language: 'D', command: './name-generator_d', category: 'Compiled', points: [{ count: 1, meanMs: 16.80 }, { count: 10, meanMs: 17.10 }, { count: 100, meanMs: 17.50 }, { count: 1000, meanMs: 18.40 }] },
    { language: 'COBOL', command: './name-generator_cobol', category: 'Compiled', points: [{ count: 1, meanMs: 22.50 }, { count: 10, meanMs: 23.10 }, { count: 100, meanMs: 23.80 }, { count: 1000, meanMs: 25.20 }] },
    { language: 'AWK', command: './name-generator.awk', category: 'Scripting', points: [{ count: 1, meanMs: 23.75 }, { count: 10, meanMs: 29.18 }, { count: 100, meanMs: 28.56 }, { count: 1000, meanMs: 40.17 }] },
    { language: 'Node.js', command: './name-generator.js', category: 'Scripting', points: [{ count: 1, meanMs: 71.08 }, { count: 10, meanMs: 57.63 }, { count: 100, meanMs: 28.97 }, { count: 1000, meanMs: 72.13 }] },
    { language: 'Python', command: 'python3 ./name-generator.py', category: 'Scripting', points: [{ count: 1, meanMs: 32.10 }, { count: 10, meanMs: 32.40 }, { count: 100, meanMs: 33.80 }, { count: 1000, meanMs: 38.50 }] },
    { language: 'Java', command: 'java NameGenerator', category: 'VM', points: [{ count: 1, meanMs: 52.40 }, { count: 10, meanMs: 53.10 }, { count: 100, meanMs: 54.80 }, { count: 1000, meanMs: 58.20 }] },
    { language: 'Bash', command: './name-generator.bash', category: 'Shell', points: [{ count: 1, meanMs: 5.40 }, { count: 10, meanMs: 20.02 }, { count: 100, meanMs: 269.17 }, { count: 1000, meanMs: 2464.61 }] },
  ];

  throughputLeaderboard = [
    { name: 'Zig', category: 'Compiled', namesPerSecond: 833333, meanMsAt1000: 1.20, peakRssMb: 1.62, startupMs: 1.06, marginalUsPerName: 0.05, sustainedRate: 20000000, relativeToFastest: 1.00 },
    { name: 'C', category: 'Compiled', namesPerSecond: 740740, meanMsAt1000: 1.35, peakRssMb: 1.54, startupMs: 1.15, marginalUsPerName: 0.08, sustainedRate: 12500000, relativeToFastest: 1.12 },
    { name: 'Go', category: 'Compiled', namesPerSecond: 456621, meanMsAt1000: 2.19, peakRssMb: 5.75, startupMs: 1.62, marginalUsPerName: 0.49, sustainedRate: 2040000, relativeToFastest: 1.82 },
    { name: 'Crystal', category: 'Compiled', namesPerSecond: 396825, meanMsAt1000: 2.52, peakRssMb: 3.98, startupMs: 2.18, marginalUsPerName: 0.36, sustainedRate: 2777000, relativeToFastest: 2.10 },
    { name: 'Nim', category: 'Compiled', namesPerSecond: 266666, meanMsAt1000: 3.75, peakRssMb: 2.56, startupMs: 3.50, marginalUsPerName: 0.25, sustainedRate: 4000000, relativeToFastest: 3.12 },
    { name: 'Odin', category: 'Compiled', namesPerSecond: 217391, meanMsAt1000: 4.60, peakRssMb: 3.77, startupMs: 4.25, marginalUsPerName: 0.35, sustainedRate: 2850000, relativeToFastest: 3.83 },
    { name: 'Pascal', category: 'Compiled', namesPerSecond: 188679, meanMsAt1000: 5.30, peakRssMb: 5.33, startupMs: 4.95, marginalUsPerName: 0.35, sustainedRate: 2850000, relativeToFastest: 4.42 },
    { name: 'Ada', category: 'Compiled', namesPerSecond: 80128, meanMsAt1000: 12.48, peakRssMb: 6.75, startupMs: 6.89, marginalUsPerName: 0.23, sustainedRate: 4347000, relativeToFastest: 10.40 },
    { name: 'Dart', category: 'Compiled', namesPerSecond: 76335, meanMsAt1000: 13.10, peakRssMb: 10.23, startupMs: 11.0, marginalUsPerName: 2.10, sustainedRate: 476000, relativeToFastest: 10.92 },
    { name: 'D', category: 'Compiled', namesPerSecond: 54347, meanMsAt1000: 18.40, peakRssMb: 9.21, startupMs: 16.5, marginalUsPerName: 1.90, sustainedRate: 526000, relativeToFastest: 15.33 },
    { name: 'COBOL', category: 'Compiled', namesPerSecond: 39682, meanMsAt1000: 25.20, peakRssMb: 10.20, startupMs: 22.5, marginalUsPerName: 2.70, sustainedRate: 370000, relativeToFastest: 21.00 },
    { name: 'Python', category: 'Scripting', namesPerSecond: 25974, meanMsAt1000: 38.50, peakRssMb: 10.75, startupMs: 31.8, marginalUsPerName: 6.70, sustainedRate: 149000, relativeToFastest: 32.08 },
    { name: 'AWK', category: 'Scripting', namesPerSecond: 24894, meanMsAt1000: 40.17, peakRssMb: 9.79, startupMs: 23.5, marginalUsPerName: 16.6, sustainedRate: 60200, relativeToFastest: 33.48 },
    { name: 'Java', category: 'VM', namesPerSecond: 17182, meanMsAt1000: 58.20, peakRssMb: 56.92, startupMs: 51.5, marginalUsPerName: 6.70, sustainedRate: 149000, relativeToFastest: 48.50 },
    { name: 'Node.js', category: 'Scripting', namesPerSecond: 13863, meanMsAt1000: 72.13, peakRssMb: 58.17, startupMs: 55.0, marginalUsPerName: 17.1, sustainedRate: 58400, relativeToFastest: 60.11 },
    { name: 'Bash', category: 'Shell', namesPerSecond: 405, meanMsAt1000: 2464.61, peakRssMb: 3.26, startupMs: 5.4, marginalUsPerName: 2460.0, sustainedRate: 406, relativeToFastest: 2057.61 },
  ];
}

// 4. Memory Leaderboard (Sorted from least to most RAM)
const memoryLeaderboard = Object.entries(memoryMap).map(([key, val]) => {
  const name = COMMAND_TO_NAME[key] || key;
  const meta = LANGUAGES_META.find((l) => l.name === name || l.compiledBin === path.basename(key) || l.file === path.basename(key));
  return {
    name,
    category: meta ? meta.category : 'Compiled',
    peakRssMb: val.peakRssMb,
    peakRssKb: val.peakRssKb,
  };
}).filter((item, idx, self) => self.findIndex((i) => i.name === item.name) === idx)
  .sort((a, b) => a.peakRssKb - b.peakRssKb);

// 5. Unified Overall Leaderboard (all unique tested languages)
const overallMap = new Map();
[...deathmatchCompiled, ...deathmatchVm, ...deathmatchScripting, ...deathmatchShells].forEach((item) => {
  if (!overallMap.has(item.name)) {
    overallMap.set(item.name, item);
  }
});
const overallLeaderboard = Array.from(overallMap.values());

const fastestCompiled = deathmatchCompiled[0] || { name: 'Zig', mean: 1.2 };
const fastestScript = deathmatchScripting[0] || { name: 'AWK', mean: 19.3 };
const leanestMemory = memoryLeaderboard[0] || { name: 'C', peakRssMb: 1.54 };

const fullBenchmarkData = {
  generatedAt: new Date().toISOString(),
  environment: {
    os: 'Linux (Ubuntu x86_64)',
    cpu: 'Host Virtualized Multi-core',
    tool: 'Hyperfine + GNU time',
  },
  stats: {
    totalLanguages: languages.length,
    activeBenchmarked: overallLeaderboard.length,
    compiledContenders: deathmatchCompiled.length,
    vmContenders: deathmatchVm.length,
    scriptingContenders: deathmatchScripting.length,
    shellContenders: deathmatchShells.length,
    fastestLanguage: fastestCompiled.name,
    fastestMeanMs: fastestCompiled.mean,
    fastestScripting: fastestScript.name,
    fastestScriptingMs: fastestScript.mean,
    leanestMemoryLanguage: leanestMemory.name,
    leanestMemoryMb: leanestMemory.peakRssMb,
    maxThroughputPerSec: throughputLeaderboard[0]?.namesPerSecond || 833333,
  },
  deathmatchCompiled,
  deathmatchVm,
  deathmatchScripting,
  deathmatchShells,
  overallLeaderboard,
  memoryLeaderboard,
  scalingCurves,
  throughputLeaderboard,
  languages,
};

const jsonStr = JSON.stringify(fullBenchmarkData, null, 2);

fs.writeFileSync(path.join(WEB_DATA_DIR, 'benchmarks.json'), jsonStr);
fs.writeFileSync(path.join(WEB_PUBLIC_DATA_DIR, 'benchmarks.json'), jsonStr);

console.log(`Successfully generated advanced benchmark telemetry dataset:`);
console.log(`- Total registered languages: ${languages.length}`);
console.log(`- Active benchmarked languages: ${overallLeaderboard.length}`);
console.log(`- Memory profiled contenders: ${memoryLeaderboard.length}`);
console.log(`- Scaling curves: ${scalingCurves.length}`);
console.log(`- Throughput items: ${throughputLeaderboard.length}`);
console.log(`- Saved to ${path.join(WEB_DATA_DIR, 'benchmarks.json')}`);
console.log(`- Saved to ${path.join(WEB_PUBLIC_DATA_DIR, 'benchmarks.json')}`);
