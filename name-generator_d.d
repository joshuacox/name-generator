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
import std.file : dirEntries, readText, SpanMode, isFile, exists;
import std.path : absolutePath, dirName;   // keep dirName for HERE, drop joinPath
import std.process : execute, environment;   // <-- new
import std.string;
import std.algorithm : filter, map, canFind, sort, min;
import std.conv : to;
import std.exception : enforce;
import std.random : uniform;
import std.array : array;
import core.stdc.stdlib : getenv;

/// Retrieve an environment variable as a D string. Returns an empty string if not set.
/// Retrieve an environment variable as a D string. Returns an empty string if not set.
string getEnv(string name) {
    // `environment` returns a `string[string]` map that is already trimmed.
    // If the key is missing we return an empty string.
    return environment.get(name, "");
}

/// Return the value of an environment variable or a default.
string envOrDefault(string name, string defaultValue) {
    auto val = getEnv(name);
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
    auto env = getEnv("counto");
    if (env.length != 0) {
        return parseIntOr(env, 24);
    }

    // Step 2: tput lines
    try {
        // `execute` runs the command and returns its stdout as a string.
        auto output = execute(["tput", "lines"]);
        // `execute` returns a tuple (status, output). Use the second element.
        return parseIntOr(strip(output[1]), 24);
    } catch (Exception) {
        // ignore – fall back
    }

    // Step 3: default
    return 24;
}

/// Return a random regular file from `folder`.
string pickRandomFile(string folder) {
    // List entries in the folder and keep only regular files.
    auto entries = dirEntries(folder, "*", SpanMode.shallow, true)
                    .filter!((e) => e.isFile)
                    .array;   // materialise as an array of DirEntry

    enforce(!entries.empty,
            "Folder `" ~ folder ~ "` contains no regular files.");

    // Pick a random index
    auto idx = uniform(0, entries.length);
    // Return the full absolute path of the selected entry
    //return folder ~ "/" ~ entries[idx].name;
    return entries[idx].name;
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
    if (getEnv("DEBUG") == "true") {
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
    immutable string HERE = dirName(__FILE__); // kept for parity with other ports
    immutable string NOUN_FOLDER = absolutePath(envOrDefault("NOUN_FOLDER", "nouns"));
    immutable string ADJ_FOLDER  = absolutePath(envOrDefault("ADJ_FOLDER",  "adjectives"));

    // Resolve noun and adjective files
    string nounFile = getEnv("NOUN_FILE");
    if (nounFile.length != 0) {
        // make the path absolute so the later read works even if cwd changes
        nounFile = absolutePath(nounFile);
        enforce(exists(nounFile) && isFile(nounFile),
                "Environment variable NOUN_FILE points to a non‑regular file: " ~ nounFile);
    } else {
        nounFile = pickRandomFile(NOUN_FOLDER);
    }

    string adjFile = getEnv("ADJ_FILE");
    if (adjFile.length != 0) {
        adjFile = absolutePath(adjFile);
        enforce(exists(adjFile) && isFile(adjFile),
                "Environment variable ADJ_FILE points to a non‑regular file: " ~ adjFile);
    } else {
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
