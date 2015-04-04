package reportingDSL

import sys.process._
import java.nio.file.Paths
import java.nio.file.Files
import java.io.File

/**
 * Singleton class with utility methods for use by Mercurial API
 */
object Utilities {

  val ONE_DAY = 24 * 60 * 60 * 60

  private def execute(command: String, cwd: String): String = {
    sys.process.Process(Seq(command), new java.io.File(cwd)).!!
  }

  /**
   * Create directory if it doesn't exist
   */
  def createDirectory(path: String) = {
    if (!fileOrDirectoryExists(path)) {
      val directory = new File(path)
      directory.mkdir()
    }
  }

  /**
   * Check if file or folder exists
   */
  def fileOrDirectoryExists(path: String) = Files.exists(Paths.get(path))

  def convertTimestampToString(timestamp: Long): String = {
    ""
  }

  def checkLastCommitDate(commitDate: Long, timePeriod: Long): Boolean = {
    false
  }

  def ensureOutputDirectoryExists(directoryLocation: String, directoryName: String): Unit = {
    val fullyQualifiedPath = directoryLocation.concat(directoryName)
    if (!fileOrDirectoryExists(fullyQualifiedPath))
      createDirectory(directoryLocation.concat(directoryName))
  }

}