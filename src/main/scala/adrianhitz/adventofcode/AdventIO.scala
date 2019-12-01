package adrianhitz.adventofcode

import java.io.{File, PrintWriter}

import scala.io.Source

/**
 * Handles reading input from and writing output to files for Advent of Code tasks. The extending class's name needs to
 * contain two digits representing the day.
 */
trait AdventIO {
  /**
   * Problem input
   */
  protected implicit val input: String = read()

  /**
   * Should solve the problems and call [[write1]] and [[write2]] to write the solutions to the output file.
   *
   * @param args Array of command line arguments for the application
   */
  def main(args: Array[String]): Unit

  /**
   * Writes the solution for part 1 to the console and an output file
   *
   * @param s Problem solution
   */
  protected[this] def write1(s: String): Unit = {
    println(f"Solution for day $fileName%s, part 1:\n$s%s\n")
    AdventIO.writeToFile(fileName + "_1", s)
  }

  /**
   * Writes the solution for part 2 to the console and an output file
   *
   * @param s Problem solution
   */
  protected[this] def write2(s: String): Unit = {
    println(f"Solution for day $fileName%s, part 2:\n$s%s\n")
    AdventIO.writeToFile(fileName + "_2", s)
  }

  /**
   * File name for this task (should be two digits, representing the day)
   */
  private def fileName: String = "\\d\\d".r.findAllIn(this.getClass.getSimpleName).toList.head

  /**
   * Reads the problem input from the input file
   *
   * @return File contents
   */
  private[this] def read(): String = {
    val source = Source.fromFile(AdventIO.inputDirectory + fileName + ".txt")
    val text = source.getLines().mkString("\n")
    source.close()
    text
  }
}

private object AdventIO {
  /**
   * Path to the directory containing the input files
   */
  private val inputDirectory = "./src/main/resources/aoc_input/"

  /**
   * Path to the directory where the output files will be created
   */
  private val outputDirectory = "./aoc_output/"

  /**
   * Writes the given content to the [[outputDirectory]], into a file with the given name. Creates the necessary
   * directories on the path if they don't already exist.
   *
   * @param fileName The file name
   * @param content  Content to be written to the file
   */
  private def writeToFile(fileName: String, content: String): Unit = {
    val directory = new File(AdventIO.outputDirectory)
    if(!directory.exists()) {
      directory.mkdirs()
    }
    new PrintWriter(new File(AdventIO.outputDirectory + fileName + ".txt")) {
      write(content)
      close()
    }
  }
}
