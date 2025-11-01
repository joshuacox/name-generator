#!/usr/bin/env node
// This was converted from the shell by llama4:scout
const fs = require('fs');
const path = require('path');
const readline = require('readline');

const HERE = process.cwd();
const NOUN_FOLDER = path.join(HERE, 'nouns');
const ADJ_FOLDER = path.join(HERE, 'adjectives');

let counto = process.env.COUNTO || 10; // Default to 10 if COUNTO is not set
let NOUN_FILE;
let ADJ_FILE;

// Function to get a random file from a directory
function getRandomFile(directory) {
  const files = fs.readdirSync(directory);
  const randomIndex = Math.floor(Math.random() * files.length);
  return path.join(directory, files[randomIndex]);
}

// Initialize NOUN_FILE and ADJ_FILE
try {
  NOUN_FILE = getRandomFile(NOUN_FOLDER);
  ADJ_FILE = getRandomFile(ADJ_FOLDER);
} catch (err) {
  console.error(err);
  process.exit(1);
}

// Function to generate a name
function generateName() {
  const noun = fs.readFileSync(NOUN_FILE, 'utf8').split('\n')[Math.floor(Math.random() * fs.readFileSync(NOUN_FILE, 'utf8').split('\n').length)].trim().toLowerCase();
  const adjective = fs.readFileSync(ADJ_FILE, 'utf8').split('\n')[Math.floor(Math.random() * fs.readFileSync(ADJ_FILE, 'utf8').split('\n').length)].trim();
  return `${adjective}-${noun}`;
}

// Main function
function main() {
  const terminalLines = process.stdout.rows;
  const count = parseInt(process.env.counto) || terminalLines; // Default to 10 if counto is not set

  for (let i = 0; i < count; i++) {
    const name = generateName();
    console.log(name);
  }
}

main();
