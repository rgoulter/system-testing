import java.io.File
package object systemTestingDSL {

  val DEFAULT_DELIMITER = "\n"
  val DEFAULT_SEPARATOR = " "
  val DEFAULT_TEST_OUTPUT_SEPARATOR = ","

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
}