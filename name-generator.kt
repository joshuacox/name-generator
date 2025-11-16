#!/usr/bin/env kotlin
// This file is intended to be compiled with Kotlin (e.g., `kotlinc name-generator.kt -include-runtime -d name-generator.jar`)
// It provides the same functionality as the script version (name-generator.kts).

import java.io.File
import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.Paths
import java.util.Random

// ---------- Helper functions ----------
fun envOrDefault(name: String, default: String): String =
    System.getenv(name) ?: default

fun envOrNull(name: String): String? = System.getenv(name)

fun toPath(str: String): Path = Paths.get(str).toAbsolutePath()

fun listRegularFiles(dir: Path): List<Path> {
    return Files.walk(dir)
        .filter { Files.isRegularFile(it) }
        .toList()
}

fun <T> randomChoice(list: List<T>, rnd: Random = Random()): T =
    list[rnd.nextInt(list.size)]

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

fun readNonEmptyLines(file: Path): List<String> {
    return Files.readAllLines(file)
        .map { it.trim() }
        .filter { it.isNotEmpty() }
}

// ---------- Main ----------
fun main() {
    // Current working directory
    val cwd: Path = Paths.get("").toAbsolutePath()

    // Configuration
    val separator: String = envOrDefault("SEPARATOR", "-")

    val countO: Int = envOrDefault("counto", "")
        .takeIf { it.isNotBlank() }
        ?.toIntOrNull()
        ?: terminalLines()

    val nounFolder: Path = envOrDefault("NOUN_FOLDER", cwd.resolve("nouns").toString())
        .let { toPath(it) }

    val adjFolder: Path = envOrDefault("ADJ_FOLDER", cwd.resolve("adjectives").toString())
        .let { toPath(it) }

    val nounFile: Path = envOrNull("NOUN_FILE")?.let { toPath(it) }
        ?: randomChoice(listRegularFiles(nounFolder))

    val adjFile: Path = envOrNull("ADJ_FILE")?.let { toPath(it) }
        ?: randomChoice(listRegularFiles(adjFolder))

    val debug: Boolean = envOrDefault("DEBUG", "false") == "true"

    val rnd = Random()

    // Load data
    val nounLines = readNonEmptyLines(nounFile)
    val adjLines = readNonEmptyLines(adjFile)

    if (nounLines.isEmpty()) {
        System.err.println("Error: noun file '$nounFile' contains no non‑empty lines.")
        kotlin.system.exitProcess(1)
    }
    if (adjLines.isEmpty()) {
        System.err.println("Error: adjective file '$adjFile' contains no non‑empty lines.")
        kotlin.system.exitProcess(1)
    }

    // Generation loop
    for (countZero in 0 until countO) {
        val nounRaw = randomChoice(nounLines, rnd)
        val adjRaw = randomChoice(adjLines, rnd)

        val noun = nounRaw.lowercase()
        val adjective = adjRaw // keep original case

        if (debug) {
            System.err.println("DEBUG:")
            System.err.println("  adjective : $adjective")
            System.err.println("  noun      : $noun")
            System.err.println("  ADJ_FILE  : $adjFile")
            System.err.println("  ADJ_FOLDER: $adjFolder")
            System.err.println("  NOUN_FILE : $nounFile")
            System.err.println("  NOUN_FOLDER: $nounFolder")
            System.err.println("  iteration : $countZero / $countO")
        }

        println("$adjective$separator$noun")
    }
}
