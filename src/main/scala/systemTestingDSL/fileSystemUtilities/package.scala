package systemTestingDSL

import java.io.File
import java.io.FileNotFoundException
package object fileSystemUtilities {

  /**
   * Convenience Method to write data to a file
   */
  def printToFile(f: java.io.File)(op: java.io.PrintWriter => Unit) {
    val p = new java.io.PrintWriter(f)
    try { op(p) } finally { p.close() }
  }

  /**
   * Returns an array containing all files starting from parent directory and traversing downwards
   */
  def getRecursiveListOfFiles(dir: File): Array[File] = {
    val these = dir.listFiles
    these ++ these.filter(_.isDirectory).flatMap(getRecursiveListOfFiles)
  }

  /**
   * Returns an array of files matching provided Regex and parent directory
   */
  def getRecursiveListOfFilesWithRegex(dir: File, regex: String): Array[File] = {
    val files = getRecursiveListOfFiles(dir)
    files.filter(file => file.getName().matches(regex))
  }

  /**
   * Reads file content as string and returns it
   */
  def readFile(path: String): String = {
    var lines = ""
    try {
      val source = scala.io.Source.fromFile(path)
      lines = source.getLines mkString DEFAULT_DELIMITER
      source.close()
    } catch {
      case ex: FileNotFoundException =>
    }
    lines
  }
}