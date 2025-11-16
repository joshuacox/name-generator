#!/usr/bin/env dmd -run
/*
 * D implementation of the name-generator script.
 *
 * Behaves like `name-generator.sh`:
 *   - Uses environment variables SEPARATOR, NOUN_FILE, ADJ_FILE,
 *     NOUN_FOLDER, ADJ_FOLDER, counto, DEBUG.
 *   - If NOUN_FILE / ADJ_FILE are not set, picks a random regular file
 *     from the respective folder.
 *   - Emits `counto` lines (default: terminal height via `tput lines`,
 *     fallback 24).
 *   - Noun is lower‑cased, adjective keeps original case.
 *   - Optional debug output when DEBUG=true.
 */

module name_generator;

import std.stdio;
import std.file;
import std.path;
import std.process;
import std.string;
import std.algorithm : filter, map, canFind, sort, min;
import std.conv : to;
import std.env;
import std.exception : enforce;
import std.random : uniform;

/// Return the value of an environment variable or a default.
string envOrDefault(string name, string defaultValue) {
    auto val = getenv(name);
    return (val.length != 0) ? val : defaultValue;
}

/// Parse an integer from a string, returning `fallback` on failure.
int parseIntOr(string s, int fallback) {
    try {
        return to!int(s);
    } catch (Exception) {
        return fallback;
    }
}

/// Determine how many lines to emit.
/// 1. If the environment variable `counto` is set, use it.
/// 2. Try `tput lines`.
/// 3. Fallback to 24.
int getCountO() {
    // Step 1: env var
    auto env = getenv("counto");
    if (env.length != 0) {
        return parseIntOr(env, 24);
    }

    // Step 2: tput lines
    try {
        auto proc = pipeProcess(["tput", "lines"], redirectStdout: true);
        proc.wait();
        if (proc.status == 0) {
            // Strip any trailing newline/whitespace from the output
            auto lineOut = proc.stdout.readLine().strip;
            return parseIntOr(lineOut, 24);
        }
    } catch (Exception) {
        // ignore – fall back
    }

    // Step 3: default
    return 24;
}

/// Return a random regular file from `folder`.
string pickRandomFile(string folder) {
    auto entries = dirEntries(folder, DirEntryType.file);
    enforce(!entries.empty, "Folder `" ~ folder ~ "` contains no regular files.");

    // Convert to array of full paths
    string[] files;
    foreach (e; entries) {
        files ~= e.path;
    }

    // Pick random index
    auto idx = uniform(0, files.length);
    return files[idx];
}

/// Read all non‑empty, trimmed lines from a file.
string[] readNonEmptyLines(string filePath) {
    auto content = readText(filePath);
    auto lines = content.splitLines
                       .map!(l => l.strip)
                       .filter!(l => l.length != 0)
                       .array;
    enforce(!lines.empty, "File `" ~ filePath ~ "` contains no non‑empty lines.");
    return lines;
}

/// Print debug information to stderr when DEBUG=true.
void maybeDebug(string adjective, string noun,
                string nounFile, string adjFile,
                string nounFolder, string adjFolder,
                size_t iteration, size_t counto) {
    if (getenv("DEBUG") == "true") {
        stderr.writeln("DEBUG iteration ", iteration, "/", counto);
        stderr.writeln("  adjective : ", adjective);
        stderr.writeln("  noun      : ", noun);
        stderr.writeln("  NOUN_FILE : ", nounFile);
        stderr.writeln("  ADJ_FILE  : ", adjFile);
        stderr.writeln("  NOUN_FOLDER: ", nounFolder);
        stderr.writeln("  ADJ_FOLDER : ", adjFolder);
    }
}

void main() {
    // Separator (default "-")
    immutable string SEPARATOR = envOrDefault("SEPARATOR", "-");

    // Determine folders (defaults relative to current working directory)
    immutable string HERE = dirName(__FILE__); // not strictly needed, but kept for parity
    immutable string NOUN_FOLDER = envOrDefault("NOUN_FOLDER", "nouns");
    immutable string ADJ_FOLDER  = envOrDefault("ADJ_FOLDER",  "adjectives");

    // Resolve noun and adjective files
    string nounFile = getenv("NOUN_FILE");
    if (nounFile.length == 0) {
        nounFile = pickRandomFile(NOUN_FOLDER);
    }
    string adjFile = getenv("ADJ_FILE");
    if (adjFile.length == 0) {
        adjFile = pickRandomFile(ADJ_FOLDER);
    }

    // Load lines
    auto nounLines = readNonEmptyLines(nounFile);
    auto adjLines  = readNonEmptyLines(adjFile);

    // Number of lines to emit
    int counto = getCountO();

    // Generation loop
    foreach (i; 0 .. counto) {
        // Pick random noun, lower‑casing it
        string nounRaw = nounLines[uniform(0, nounLines.length)];
        string noun = nounRaw.toLower();

        // Pick random adjective (preserve case)
        string adjective = adjLines[uniform(0, adjLines.length)];

        maybeDebug(adjective, noun,
                   nounFile, adjFile,
                   NOUN_FOLDER, ADJ_FOLDER,
                   i, counto);

        // Output
        writeln(adjective ~ SEPARATOR ~ noun);
    }
}
