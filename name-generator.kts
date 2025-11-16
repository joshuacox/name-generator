#!/usr/bin/env kotlin

import java.io.File
import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.Paths
import java.util.Random

// ---------- Helper functions ----------
fun envOrDefault(name: String, default: String): String =
    System.getenv(name) ?: default

fun envOrNull(name: String): String? = System.getenv(name)

// Resolve a path to an absolute Path (no canonicalisation needed for our purposes)
fun toPath(str: String): Path = Paths.get(str).toAbsolutePath()

// Return a list of all regular files under a directory (recursively)
fun listRegularFiles(dir: Path): List<Path> {
    return Files.walk(dir)
        .filter { Files.isRegularFile(it) }
        .toList()
}

// Pick a random element from a list
fun <T> randomChoice(list: List<T>, rnd: Random = Random()): T =
    list[rnd.nextInt(list.size)]

// Try to obtain terminal height via `tput lines`; fall back to 24
fun terminalLines(): Int {
    return try {
        val proc = ProcessBuilder("tput", "lines")
            .redirectErrorStream(true)
            .start()
        val output = proc.inputStream.bufferedReader().readText().trim()
        proc.waitFor()
        output.toIntOrNull() ?: 24
    } catch (e: Exception) {
        24
    }
}

// Read all non‑empty, trimmed lines from a file
fun readNonEmptyLines(file: Path): List<String> {
    return Files.readAllLines(file)
        .map { it.trim() }
        .filter { it.isNotEmpty() }
}

// ---------- Configuration ----------
val cwd: Path = Paths.get("").toAbsolutePath()

val SEPARATOR: String = envOrDefault("SEPARATOR", "-")

val COUNT_O: Int = envOrDefault("counto", "")
    .takeIf { it.isNotBlank() }
    ?.toIntOrNull()
    ?: terminalLines()

val NOUN_FOLDER: Path = envOrDefault("NOUN_FOLDER", cwd.resolve("nouns").toString())
    .let { toPath(it) }

val ADJ_FOLDER: Path = envOrDefault("ADJ_FOLDER", cwd.resolve("adjectives").toString())
    .let { toPath(it) }

val NOUN_FILE: Path = envOrNull("NOUN_FILE")?.let { toPath(it) }
    ?: randomChoice(listRegularFiles(NOUN_FOLDER))

val ADJ_FILE: Path = envOrNull("ADJ_FILE")?.let { toPath(it) }
    ?: randomChoice(listRegularFiles(ADJ_FOLDER))

val DEBUG: Boolean = envOrDefault("DEBUG", "false") == "true"

val rnd = Random()

// ---------- Load data ----------
val nounLines = readNonEmptyLines(NOUN_FILE)
val adjLines = readNonEmptyLines(ADJ_FILE)

if (nounLines.isEmpty()) {
    System.err.println("Error: noun file '${NOUN_FILE}' contains no non‑empty lines.")
    kotlin.system.exitProcess(1)
}
if (adjLines.isEmpty()) {
    System.err.println("Error: adjective file '${ADJ_FILE}' contains no non‑empty lines.")
    kotlin.system.exitProcess(1)
}

// ---------- Main generation loop ----------
for (countZero in 0 until COUNT_O) {
    // Pick random noun and adjective
    val nounRaw = randomChoice(nounLines, rnd)
    val adjRaw = randomChoice(adjLines, rnd)

    val noun = nounRaw.lowercase()
    val adjective = adjRaw // keep original case

    if (DEBUG) {
        System.err.println("DEBUG:")
        System.err.println("  adjective : $adjective")
        System.err.println("  noun      : $noun")
        System.err.println("  ADJ_FILE  : $ADJ_FILE")
        System.err.println("  ADJ_FOLDER: $ADJ_FOLDER")
        System.err.println("  NOUN_FILE : $NOUN_FILE")
        System.err.println("  NOUN_FOLDER: $NOUN_FOLDER")
        System.err.println("  iteration : $countZero / $COUNT_O")
    }

    println("$adjective$SEPARATOR$noun")
}
