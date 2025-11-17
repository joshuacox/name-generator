#!/usr/bin/env node
import fs from 'fs';
import path from 'path';

// ------------------------------------------------------------------------------- //
// Configuration – environment overrides with sensible defaults
// ------------------------------------------------------------------------------- //
const SEPARATOR: string = process.env.SEPARATOR || "-";
const NOUN_FOLDER: string = process.env.NOUN_FOLDER || path.join(process.cwd(), "nouns");
const ADJ_FOLDER: string = process.env.ADJ_FOLDER || path.join(process.cwd(), "adjectives");

// Add new helper function for resolving files
function resolveFilePath(envVar: string, defaultFolder: string): string {
    const envVal = process.env[envVar];
    if (envVal) {
        // If the env var points to a directory, throw error like shell script
        if (fs.existsSync(envVal) && fs.lstatSync(envVal).isDirectory()) {
            throw new Error(`Environment variable ${envVar} points to a directory`);
        }
        return path.resolve(envVal);
    }
    // If no env var, return random file from default folder
    const files = fs.readdirSync(defaultFolder)
                    .filter(f => fs.lstatSync(path.join(defaultFolder, f)).isFile());
    if (files.length === 0) {
        throw new Error(`Folder ${defaultFolder} contains no regular files`);
    }
    return path.join(defaultFolder, files[Math.floor(Math.random() * files.length)]);
}

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
    // Get noun and adjective files based on environment variables or random selection
    const nounFile = resolveFilePath('NOUN_FILE', NOUN_FOLDER);
    const adjFile = resolveFilePath('ADJ_FILE', ADJ_FOLDER);

    // Read lines from selected files
    const nouns = readNonEmptyLines(nounFile);
    const adjectives = readNonEmptyLines(adjFile);

    // Determine how many names to generate
    // Determine how many names to generate (using tput lines logic)
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
