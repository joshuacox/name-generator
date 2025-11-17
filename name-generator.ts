#!/usr/bin/env node
import fs from 'fs';
import path from 'path';

// ------------------------------------------------------------------------------- //
// Configuration – environment overrides with sensible defaults
// ------------------------------------------------------------------------------- //
const SEPARATOR: string = process.env.SEPARATOR || "-";
const NOUN_FOLDER: string = process.env.NOUN_FOLDER || path.join(process.cwd(), "nouns");
const ADJ_FOLDER: string = process.env.ADJ_FOLDER || path.join(process.cwd(), "adjectives");

// ------------------------------------------------------------------------------- //
// Helper functions
// ------------------------------------------------------------------------------- //

/**
 * Resolves an environment variable to a value, with fallback to default
 */
function envOrDefault(name: string, defaultValue: string): string {
    return process.env[name] || defaultValue;
}

/**
 * Reads non-empty lines from a file
 */
function readNonEmptyLines(filePath: string): string[] {
    const content = fs.readFileSync(filePath, 'utf8');
    return content.split('\n')
        .map(line => line.trim())
        .filter(line => line.length > 0);
}

/**
 * Picks a random line from an array of lines
 */
function pickRandomLine(lines: string[]): string {
    if (lines.length === 0) throw new Error("No lines to choose from");
    const randomIndex = Math.floor(Math.random() * lines.length);
    return lines[randomIndex];
}

// ------------------------------------------------------------------------------- //
// Main generation logic
// ------------------------------------------------------------------------------- //

/**
 * Gets the number of lines to output, using tput lines if available
 */
function getCountO(): number {
    // Try to get count from environment variable first
    const envCount = process.env.counto;
    if (envCount) {
        try {
            return parseInt(envCount, 10);
        } catch (e) {
            // Ignore parse error and fall through
        }
    }

    // Fallback to default value
    return 24; // Same as shell script's fallback
}

/**
 * Outputs debug information if DEBUG environment variable is set
 */
function maybeDebug(adjective: string, noun: string): void {
    if (process.env.DEBUG === "true") {
        console.error("DEBUG:");
        console.error(`Adjective: ${adjective}`);
        console.error(`Noun: ${noun}`);
        console.error(`NOUN_FOLDER: ${NOUN_FOLDER}`);
        console.error(`ADJ_FOLDER: ${ADJ_FOLDER}`);
    }
}

/**
 * Main execution function
 */
async function main(): Promise<void> {
    // Read directories for noun and adjective files
    const nounFiles = await fs.promises.readdir(NOUN_FOLDER);
    const adjFiles = await fs.promises.readdir(ADJ_FOLDER);

    if (nounFiles.length === 0 || adjFiles.length === 0) {
        throw new Error("No files found in folders");
    }

    // Select random files
    const nounFile = path.join(NOUN_FOLDER, nounFiles[Math.floor(Math.random() * nounFiles.length)]);
    const adjFile = path.join(ADJ_FOLDER, adjFiles[Math.floor(Math.random() * adjFiles.length)]);

    // Read lines from selected files
    const nouns = readNonEmptyLines(nounFile);
    const adjectives = readNonEmptyLines(adjFile);

    // Determine how many names to generate
    const counto = getCountO();

    for (let countzero = 0; countzero < counto; countzero++) {
        const randomAdjective = pickRandomLine(adjectives);
        let randomNoun = pickRandomLine(nouns).toLowerCase();

        maybeDebug(randomAdjective, randomNoun);

        console.log(`${randomAdjective}${SEPARATOR}${randomNoun}`);
    }
}

// Start the generation process
main().catch(err => {
    console.error("Error:", err);
    process.exit(1);
});
