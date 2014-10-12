package object systemTestingDSL {

  val DEFAULT_DELIMITER = "\n"
  val DEFAULT_SEPARATOR = " "

  // Convenience Method to write data to a file
  def printToFile(f: java.io.File)(op: java.io.PrintWriter => Unit) {
    val p = new java.io.PrintWriter(f)
    try { op(p) } finally { p.close() }
  }
}