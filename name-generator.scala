#!/usr/bin/env scala
import java.io.File
import java.util.Random
import scala.io.Source
import scala.util.Try

object NameGenerator {
  private val random = new Random()

  // -------------------------------------------------------------------------------//
  // Configuration – values are taken from the environment, otherwise a default
  // -------------------------------------------------------------------------------//
  val HERE: File = new File(System.getProperty("user.dir")).getCanonicalFile
  
  // Load configuration from **environment variables** (fallbacks match the shell script)
  val SEPARATOR: String = sys.env.getOrElse("SEPARATOR", "-")
  val NOUN_FOLDER: File = getEnvAsFile("NOUN_FOLDER", new File(HERE, "nouns"))
  val ADJ_FOLDER: File = getEnvAsFile("ADJ_FOLDER", new File(HERE, "adjectives"))

  // Get the number of lines to generate 
  val COUNT_O: Int = getCountO()

  // -------------------------------------------------------------------------------//
  // Helper methods
  // -------------------------------------------------------------------------------//

  /** Resolve an environment variable to a regular file.
    * If the variable is unset/empty → return the supplied `default` folder.
    * If it is set → return the canonical file (must exist and be a regular file). */
  private def getEnvAsFile(varName: String, default: File): File = {
    sys.env.get(varName) match {
      case Some(p) if p.trim.nonEmpty =>
        val f = new File(p).getCanonicalFile
        if (!f.isFile) throw new IllegalStateException(
          s"Environment variable $varName points to a non‑regular file: $p")
        f
      case _ => default
    }
  }

  private def getCountO(): Int = {
    // 1️⃣ Environment variable `counto` (preferred)
    sys.env.get("counto").flatMap(s => Try(s.toInt).toOption) match {
      case Some(v) => return v
      case None    => // continue
    }

    // 2️⃣ Try `tput lines`
    try {
      val proc = new ProcessBuilder("tput", "lines").start()
      val out = Source.fromInputStream(proc.getInputStream).mkString.trim
      if (out.matches("\\d+")) return out.toInt
    } catch {
      case _: Exception => // ignore
    }

    // 3️⃣ Fallback to 24 (same as the shell script)
    24
  }

  private def listRegularFiles(folder: File): List[File] = {
    folder.listFiles
      .filter(_ != null)
      .filter(_.isFile)
      .toList
  }

  private def pickRandomFile(folder: File): File = {
    val files = listRegularFiles(folder)
    if (files.isEmpty) {
      throw new IllegalStateException(s"Folder ${folder.getPath} contains no regular files")
    }
    files(random.nextInt(files.size))
  }

  private def readNonEmptyLines(file: File): List[String] = {
    Source
      .fromFile(file)
      .getLines()
      .map(_.trim)
      .filterNot(_.isEmpty)
      .toList
  }

  // -------------------------------------------------------------------------------//
  // Main generation logic
  // -------------------------------------------------------------------------------//

  private def maybeDebug(adjective: String, noun: String, nounFile: File, adjFile: File): Unit = {
    if (sys.env.get("DEBUG").contains("true")) {
      System.err.println("DEBUG:")
      System.err.println(s"Adjective: $adjective")
      System.err.println(s"Noun: $noun")
      System.err.println(s"NOUN_FILE: ${nounFile.getPath}")
      System.err.println(s"ADJ_FILE: ${adjFile.getPath}")
      System.err.println(s"NOUN_FOLDER: ${NOUN_FOLDER.getPath}")
      System.err.println(s"ADJ_FOLDER: ${ADJ_FOLDER.getPath}")
      System.err.println(s"SEPARATOR: $SEPARATOR")
    }
  }

  // Resolve a file from an env‑var or pick a random regular file from `folder`.
  private def resolveFile(envVar: String, folder: File): File = {
    sys.env.get(envVar) match {
      case Some(p) if p.trim.nonEmpty =>
        val f = new File(p).getCanonicalFile
        if (!f.isFile) throw new IllegalStateException(
          s"Environment variable $envVar points to a non‑regular file: $p")
        f
      case _ => pickRandomFile(folder)
    }
  }

  def generateNames(): Unit = {
    // Resolve files – env var overrides, otherwise pick a random file from the folder
    val nounFile = resolveFile("NOUN_FILE", NOUN_FOLDER)
    val adjFile = resolveFile("ADJ_FILE", ADJ_FOLDER)

    // Rest of the method remains the same...
    val nouns = readNonEmptyLines(nounFile)
    val adjectives = readNonEmptyLines(adjFile)

    var count = 0
    while (count < COUNT_O) {
      // Pick random entries preserving case
      val noun = nouns(random.nextInt(nouns.size)).toLowerCase()
      val adjective = adjectives(random.nextInt(adjectives.size))
      
      maybeDebug(adjective, noun, nounFile, adjFile)
      
      println(s"$adjective$SEPARATOR$noun")
      
      count += 1
    }
  }

  // -------------------------------------------------------------------------------//
  // Entry point
  // -------------------------------------------------------------------------------//

  def main(args: Array[String]): Unit = {
    generateNames()
  }
}
