#!/usr/bin/env scala
import java.io.File
import java.util.Random
import scala.io.Source

object NameGenerator {
  private val random = new Random()

  // -------------------------------------------------------------------------------//
  // Configuration – values are taken from the environment, otherwise a default
  // -------------------------------------------------------------------------------//
  val HERE: File = new File(System.getProperty("user.dir")).getCanonicalFile
  
  // Extract SEPARATOR from environment
  val SEPARATOR: String = System.getProperty("SEPARATOR", "-")

  // Load environment variables with fallbacks
  val NOUN_FOLDER: File = getEnvAsFile("NOUN_FOLDER", new File(HERE, "nouns"))
  val ADJ_FOLDER: File = getEnvAsFile("ADJ_FOLDER", new File(HERE, "adjectives"))

  // Get the number of lines to generate 
  val COUNT_O: Int = getCountO()

  // -------------------------------------------------------------------------------//
  // Helper methods
  // -------------------------------------------------------------------------------//

  private def getEnvAsFile(varName: String, default: File): File = {
    System.getProperty(varName) match {
      case null | "" => default
      case path => new File(path).getCanonicalFile
    }
  }

  private def getCountO(): Int = {
    // 1️⃣ First check for counto environment variable (preferred)
    //val envCount = System.getProperty("counto")
    val envCount = sys.env.get("counto")
    if (envCount != null) {
      try {
        return Integer.parseInt(envCount)
      } catch {
        case _: NumberFormatException =>
      }
    }

    // // 2️⃣ Then try to get from tput lines
    // try {
    //   val processBuilder = new ProcessBuilder("tput", "lines")
    //   val process = processBuilder.start()
    //   val output = Source.fromInputStream(process.getInputStream).mkString.trim
    //   if (output.matches("\\d+")) {
    //     return Integer.parseInt(output)
    //   }
    // } catch {
    //   case _: Exception =>
    // }
    //
    // // 3️⃣ Final fallback to 24 if all else fails
    // return 2
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

  private def maybeDebug(adjective: String, noun: String): Unit = {
    if (System.getProperty("DEBUG") == "true") {
      System.err.println("DEBUG:")
      System.err.println(s"Adjective: $adjective")
      System.err.println(s"Noun: $noun")
      System.err.println(s"NOUN_FOLDER: ${NOUN_FOLDER.getPath}")
      System.err.println(s"ADJ_FOLDER: ${ADJ_FOLDER.getPath}")
    }
  }

  def generateNames(): Unit = {
    // Resolve files - env var overrides, otherwise pick a random file from the folder
    val nounFile = Option(System.getProperty("NOUN_FILE")).map(new File(_).getCanonicalFile).getOrElse(pickRandomFile(NOUN_FOLDER))
    val adjFile = Option(System.getProperty("ADJ_FILE")).map(new File(_).getCanonicalFile).getOrElse(pickRandomFile(ADJ_FOLDER))

    val nouns = readNonEmptyLines(nounFile)
    val adjectives = readNonEmptyLines(adjFile)

    var count = 0
    while (count < COUNT_O) {
      // Pick random entries preserving case
      val noun = nouns(random.nextInt(nouns.size)).toLowerCase()
      val adjective = adjectives(random.nextInt(adjectives.size))
      
      maybeDebug(adjective, noun)
      
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
