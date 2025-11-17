#!/usr/bin/env php
<?php
/**
 * PHP implementation of the original `name-generator.sh`.
 *
 * Behaviour:
 *   * Uses the environment variables SEPARATOR, NOUN_FILE, ADJ_FILE,
 *     NOUN_FOLDER, ADJ_FOLDER and counto (the number of lines to emit).
 *   * If NOUN_FILE / ADJ_FILE are not set, a random regular file is chosen
 *     from the respective folder.
 *   * If a folder variable is not set, it defaults to "<cwd>/nouns" and
 *     "<cwd>/adjectives".
 *   * The noun is lower‑cased, the adjective keeps its original case.
 *   * When DEBUG=true a small debug dump is written to STDERR.
 *   * No greeting is added – the output matches the original shell script
 *     (e.g., "test_test").
 */

declare(strict_types=1);

/* --------------------------------------------------- *
 * Helper utilities
 * --------------------------------------------------- */

/**
 * Return true if $path points to a regular file.
 */
function is_regular_file(string $path): bool
{
    return is_file($path) && is_readable($path);
}

/**
 * Pick a random regular file from $folder.
 *
 * @throws RuntimeException if the folder contains no regular files.
 */
function pick_random_file(string $folder): string
{
    $entries = scandir($folder, SCANDIR_SORT_NONE);
    if ($entries === false) {
        throw new RuntimeException("Cannot read directory: $folder");
    }

    $files = [];
    foreach ($entries as $entry) {
        if ($entry === '.' || $entry === '..') {
            continue;
        }
        $full = $folder . DIRECTORY_SEPARATOR . $entry;
        if (is_regular_file($full)) {
            $files[] = $full;
        }
    }

    if (count($files) === 0) {
        throw new RuntimeException("Folder $folder does not contain any regular files.");
    }

    return $files[random_int(0, count($files) - 1)];
}

/**
 * Resolve a file path from an environment variable or, if not set,
 * pick a random file from $folder.
 *
 * @throws RuntimeException if the env‑var points to a non‑regular file.
 */
function resolve_file(string $envVar, string $folder): string
{
    $val = getenv($envVar);
    if ($val !== false && trim($val) !== '') {
        $path = realpath($val);
        if ($path === false || !is_regular_file($path)) {
            throw new RuntimeException(
                "Environment variable $envVar points to a non‑regular file: $val"
            );
        }
        return $path;
    }

    return pick_random_file($folder);
}

/**
 * Read all non‑empty, trimmed lines from $file.
 *
 * @return list<string>
 */
function read_nonempty_lines(string $file): array
{
    $raw = file($file, FILE_IGNORE_NEW_LINES);
    if ($raw === false) {
        throw new RuntimeException("Cannot read file: $file");
    }

    $lines = [];
    foreach ($raw as $line) {
        $trim = trim($line);
        if ($trim !== '') {
            $lines[] = $trim;
        }
    }
    return $lines;
}

/**
 * Return the number of lines to emit.
 *
 * 1. Environment variable `counto` (if numeric).
 * 2. `tput lines` command (if available).
 * 3. Fallback to 24.
 */
function get_count_o(): int
{
    $env = getenv('counto');
    if ($env !== false && is_numeric($env)) {
        return (int)$env;
    }

    // Try `tput lines`
    $output = null;
    $returnVar = null;
    @exec('tput lines 2>/dev/null', $output, $returnVar);
    if ($returnVar === 0 && isset($output[0]) && is_numeric(trim($output[0]))) {
        return (int)trim($output[0]);
    }

    return 24;
}

/* --------------------------------------------------- *
 * Configuration (environment → defaults)
 * --------------------------------------------------- */

$HERE = getcwd();

$SEPARATOR = getenv('SEPARATOR') ?: '-';
$COUNT_O   = get_count_o();

$NOUN_FOLDER = getenv('NOUN_FOLDER')
    ?: $HERE . DIRECTORY_SEPARATOR . 'nouns';
$ADJ_FOLDER  = getenv('ADJ_FOLDER')
    ?: $HERE . DIRECTORY_SEPARATOR . 'adjectives';

$NOUN_FILE = resolve_file('NOUN_FILE', $NOUN_FOLDER);
$ADJ_FILE  = resolve_file('ADJ_FILE',  $ADJ_FOLDER);

/* --------------------------------------------------- *
 * Load the word lists
 * --------------------------------------------------- */

$nounLines = read_nonempty_lines($NOUN_FILE);
$adjLines  = read_nonempty_lines($ADJ_FILE);

if (count($nounLines) === 0) {
    fwrite(STDERR, "Error: noun list is empty.\n");
    exit(1);
}
if (count($adjLines) === 0) {
    fwrite(STDERR, "Error: adjective list is empty.\n");
    exit(1);
}

/* --------------------------------------------------- *
 * Main generation loop
 * --------------------------------------------------- */

$debug = getenv('DEBUG') === 'true';

for ($i = 0; $i < $COUNT_O; $i++) {
    // Pick random entries
    $nounRaw = $nounLines[random_int(0, count($nounLines) - 1)];
    $adjRaw  = $adjLines[random_int(0, count($adjLines) - 1)];

    $noun = strtolower($nounRaw);
    $adj  = $adjRaw; // keep original case

    if ($debug) {
        fwrite(STDERR, "DEBUG:\n");
        fwrite(STDERR, "  adjective : $adj\n");
        fwrite(STDERR, "  noun      : $noun\n");
        fwrite(STDERR, "  ADJ_FILE  : $ADJ_FILE\n");
        fwrite(STDERR, "  ADJ_FOLDER: $ADJ_FOLDER\n");
        fwrite(STDERR, "  NOUN_FILE : $NOUN_FILE\n");
        fwrite(STDERR, "  NOUN_FOLDER: $NOUN_FOLDER\n");
        fwrite(STDERR, "  $i > $COUNT_O\n");
    }

    // Output without any greeting, matching the original script
    echo $adj . $SEPARATOR . $noun . PHP_EOL;
}
