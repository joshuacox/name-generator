#!/usr/bin/env node
// This was originally converted from the shell by llama4:scout
const fs = require('fs').promises;
const path = require('path');

const HERE = process.cwd();
const NOUN_FOLDER = path.join(HERE, 'nouns');
const ADJ_FOLDER = path.join(HERE, 'adjectives');

let counto = 10; // Default to 10

// Function to get a random file from a directory
async function getRandomFile(directory) {
  const files = await fs.readdir(directory);
  const randomIndex = Math.floor(Math.random() * files.length);
  return path.join(directory, files[randomIndex]);
}

// Function to read a file
async function readFile(filePath) {
  return await fs.readFile(filePath, 'utf8');
}

// Function to generate a name
async function generateName() {
  try {
    const NOUN_FILE = await getRandomFile(NOUN_FOLDER);
    const ADJ_FILE = await getRandomFile(ADJ_FOLDER);

    const nounFileContent = await readFile(NOUN_FILE);
    const adjFileContent = await readFile(ADJ_FILE);

    const nouns = nounFileContent.split('\n');
    const adjectives = adjFileContent.split('\n');

    const randomNounIndex = Math.floor(Math.random() * nouns.length);
    const randomAdjectiveIndex = Math.floor(Math.random() * adjectives.length);

    const noun = nouns[randomNounIndex].trim().toLowerCase();
    const adjective = adjectives[randomAdjectiveIndex].trim();

    return `${adjective}-${noun}`;
  } catch (err) {
    console.error(err);
  }
}

const { exec } = require('child_process');

// Main function
async function main() {
  var linesss = 0;
  try {
    const terminalLines = process.stdout.rows;
    const count = parseInt(process.env.counto) || terminalLines; // Default to 10 if counto is not set
    for (let i = 0; i < count; i++) {
      const name = await generateName();
      console.log(name);
    }
  } catch (err) {
    console.error(err);
  }
}

main();
