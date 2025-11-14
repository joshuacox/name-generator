#!/usr/bin/env node
// Updated to match the behaviour of name-generator.sh
// - Select a single noun file and adjective file for the whole run
// - Use SEPARATOR env var (default "-")
// - Default count to terminal height (tput lines) if counto not set
// - Provide DEBUG output similar to the shell script

const fs = require('fs').promises;
const path = require('path');
const { execSync } = require('child_process');

// Resolve base directories
const HERE = process.cwd();
const NOUN_FOLDER = path.join(HERE, 'nouns');
const ADJ_FOLDER = path.join(HERE, 'adjectives');

// Environment configuration (with fall‑backs)
const SEPARATOR = process.env.SEPARATOR || '-';
const DEBUG = process.env.DEBUG === 'true';

// Helper: pick a random file from a directory
async function getRandomFile(directory) {
  const files = await fs.readdir(directory);
  if (files.length === 0) {
    throw new Error(`No files found in directory ${directory}`);
  }
  const randomIndex = Math.floor(Math.random() * files.length);
  return path.join(directory, files[randomIndex]);
}

// Helper: read a file and split into non‑empty trimmed lines
async function readLines(filePath) {
  const content = await fs.readFile(filePath, 'utf8');
  return content
    .split('\n')
    .map(l => l.trim())
    .filter(l => l.length > 0);
}

// Debug output mirroring the shell script's `debugger` function
function debuggerInfo({
  adjective,
  noun,
  nounFile,
  adjFile,
  nounFolder,
  adjFolder,
  countzero,
  counto,
}) {
  if (!DEBUG) return;
  console.error('DEBUG:');
  console.error(`  adjective : ${adjective}`);
  console.error(`  noun      : ${noun}`);
  console.error(`  ADJ_FILE  : ${adjFile}`);
  console.error(`  ADJ_FOLDER: ${adjFolder}`);
  console.error(`  NOUN_FILE : ${nounFile}`);
  console.error(`  NOUN_FOLDER: ${nounFolder}`);
  console.error(`  ${countzero} > ${counto}`);
}

// Generate a single name using pre‑selected files
async function generateName(nounLines, adjLines) {
  const noun = nounLines[Math.floor(Math.random() * nounLines.length)].toLowerCase();
  const adjective = adjLines[Math.floor(Math.random() * adjLines.length)];
  return `${adjective}${SEPARATOR}${noun}`;
}

// Main execution flow
(async () => {
  try {
    // Determine how many names to emit.
    // Shell script uses `tput lines`; we approximate with process.stdout.rows,
    // falling back to 24 if unavailable.
    const terminalLines = process.stdout.rows || 24;
    const countEnv = process.env.counto;
    const count = countEnv ? parseInt(countEnv, 10) : terminalLines;

    // Resolve the single noun and adjective files (once per run)
    const NOUN_FILE = await getRandomFile(NOUN_FOLDER);
    const ADJ_FILE = await getRandomFile(ADJ_FOLDER);

    // Load their contents once
    const [nounLines, adjLines] = await Promise.all([
      readLines(NOUN_FILE),
      readLines(ADJ_FILE),
    ]);

    // Emit names
    for (let i = 0; i < count; i++) {
      const name = await generateName(nounLines, adjLines);
      // Split back to parts for debugging (mirrors shell behaviour)
      const [adjPart, nounPart] = name.split(SEPARATOR);
      debuggerInfo({
        adjective: adjPart,
        noun: nounPart,
        nounFile: NOUN_FILE,
        adjFile: ADJ_FILE,
        nounFolder: NOUN_FOLDER,
        adjFolder: ADJ_FOLDER,
        countzero: i,
        counto: count,
      });
      console.log(name);
    }
  } catch (err) {
    console.error('Error:', err);
    process.exit(1);
  }
})();
