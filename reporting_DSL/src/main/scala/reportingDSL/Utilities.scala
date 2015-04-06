package reportingDSL

import sys.process._
import java.nio.file.Paths
import java.nio.file.Files
import java.io.File
import java.util.Date
import java.io.PrintWriter

/**
 * Singleton class with utility methods for use by Mercurial API
 */
object Utilities {

  val ONE_DAY = 24 * 60 * 60
  val SECONDS_TO_MILLI = 1000

  /**
   * Create directory if it doesn't exist
   */
  def createDirectory(path: String) = {
    if (!fileOrDirectoryExists(path)) {
      val directory = new File(path)
      directory.mkdirs()
    }
  }

  /**
   * Check if file or folder exists
   */
  def fileOrDirectoryExists(path: String) = Files.exists(Paths.get(path))

  def convertTimestampToString(timestamp: String): String = {
    convertTimestampToString(timestamp.toLong)
  }
  def convertTimestampToString(timestamp: Long): String = {
    new Date(timestamp * this.SECONDS_TO_MILLI).toString()
  }

  /**
   * Checks if the date of particular commit is within the given time period
   * NOTE: Time Period must be in days
   */
  def checkLastCommitDate(commitDate: Long, timePeriod: Long): Boolean = {
    val currentTime: Long = System.currentTimeMillis() / this.SECONDS_TO_MILLI
    (currentTime - commitDate) <= (timePeriod * this.ONE_DAY)
  }

  def checkLastCommitDate(commitDate: String, timePeriod: Long): Boolean = {
    checkLastCommitDate(commitDate.toLong, timePeriod)
  }

  def ensureOutputDirectoryExists(directoryLocation: String, directoryName: String): Unit = {
    val fullyQualifiedPath = directoryLocation.concat(directoryName)
    if (!fileOrDirectoryExists(fullyQualifiedPath))
      createDirectory(directoryLocation.concat(directoryName))
  }

  def printToFile(f: java.io.File)(op: PrintWriter => Unit) {
    val p = new java.io.PrintWriter(f)
    try { op(p) } finally { p.close() }
  }

  def writeToFile(file: String, content: String) = {
    printToFile(new File(file))(_.print(content))
  }

}