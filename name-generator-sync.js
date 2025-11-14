#!/usr/bin/env node
// Synchronous version of the name generator.
// Behaves like the original shell script (name-generator.sh) with respect to
// environment variables NOUN_FILE, ADJ_FILE, NOUN_FOLDER, ADJ_FOLDER, counto,
// SEPARATOR and DEBUG.

const fs = require('fs');
const path = require('path');

// -----------------------------------------------------------------------------
// Configuration (environment overrides, defaults)
// -----------------------------------------------------------------------------
const HERE = process.cwd();

// Folder locations – can be overridden by environment variables.
const NOUN_FOLDER = process.env.NOUN_FOLDER
  ? path.resolve(process.env.NOUN_FOLDER)
  : path.join(HERE, 'nouns');

const ADJ_FOLDER = process.env.ADJ_FOLDER
  ? path.resolve(process.env.ADJ_FOLDER)
  : path.join(HERE, 'adjectives');

// Separator between adjective and noun (default "-").
const SEPARATOR = process.env.SEPARATOR || '-';

// -----------------------------------------------------------------------------
// Resolve files – honour NOUN_FILE / ADJ_FILE env vars, otherwise pick random.
// -----------------------------------------------------------------------------
function getRandomFile(directory) {
  const files = fs.readdirSync(directory);
  if (files.length === 0) {
    throw new Error(`No files found in directory ${directory}`);
  }
  const randomIndex = Math.floor(Math.random() * files.length);
  return path.join(directory, files[randomIndex]);
}

// If the user supplied a specific file via env var, use it; otherwise pick one.
const NOUN_FILE = process.env.NOUN_FILE
  ? path.resolve(process.env.NOUN_FILE)
  : getRandomFile(NOUN_FOLDER);

const ADJ_FILE = process.env.ADJ_FILE
  ? path.resolve(process.env.ADJ_FILE)
  : getRandomFile(ADJ_FOLDER);

// -----------------------------------------------------------------------------
// Debug helper – prints information when DEBUG=true
// -----------------------------------------------------------------------------
function debuggerInfo(adjective, noun) {
  if (process.env.DEBUG === 'true') {
    console.error('DEBUG:');
    console.error(`  adjective : ${adjective}`);
    console.error(`  noun      : ${noun}`);
    console.error(`  ADJ_FILE  : ${ADJ_FILE}`);
    console.error(`  ADJ_FOLDER: ${ADJ_FOLDER}`);
    console.error(`  NOUN_FILE : ${NOUN_FILE}`);
    console.error(`  NOUN_FOLDER: ${NOUN_FOLDER}`);
  }
}

// -----------------------------------------------------------------------------
// Helper to pick a random line from a file (mimics `shuf -n 1`)
// -----------------------------------------------------------------------------
function pickRandomLine(filePath) {
  const content = fs.readFileSync(filePath, 'utf8');
  const lines = content.split('\n');
  // Choose a random index; if the line is empty, keep trying a few times.
  let line = '';
  for (let attempts = 0; attempts < 5 && line === ''; attempts++) {
    line = lines[Math.floor(Math.random() * lines.length)];
  }
  return line.trim();
}

// -----------------------------------------------------------------------------
// Generate a single name
// -----------------------------------------------------------------------------
function generateName() {
  const nounRaw = pickRandomLine(NOUN_FILE);
  const adjRaw = pickRandomLine(ADJ_FILE);

  const noun = nounRaw.toLowerCase();
  const adjective = adjRaw; // keep original case as in the shell script

  debuggerInfo(adjective, noun);
  return `${adjective}${SEPARATOR}${noun}`;
}

// -----------------------------------------------------------------------------
// Main execution
// -----------------------------------------------------------------------------
function main() {
  // Determine how many names to emit.
  // The original script uses `tput lines` (terminal height) as default.
  const terminalLines = process.stdout.rows || 24;
  const countEnv = process.env.counto;
  const count = countEnv ? parseInt(countEnv, 10) : terminalLines;

  const iterations = Number.isNaN(count) ? terminalLines : count;

  for (let i = 0; i < iterations; i++) {
    console.log(generateName());
  }
}

main();
