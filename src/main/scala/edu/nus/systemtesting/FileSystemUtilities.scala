package edu.nus.systemtesting

import java.io.File
import java.io.FileNotFoundException
import java.nio.file.Files
import java.nio.file.FileVisitResult
import java.nio.file.Path
import java.nio.file.Paths
import java.nio.file.SimpleFileVisitor
import java.text.SimpleDateFormat
import java.util.Date
import java.nio.file.attribute.BasicFileAttributes
import java.io.IOException

/**
 * This object provides some convenient methods/functions to provide fileSystem
 * related operations
 */
object FileSystemUtilities {
  /**
   * Convenience Method to write data to a file
   */
  def printToFile(f: File)(op: java.io.PrintWriter => Unit) {
    val p = new java.io.PrintWriter(f)
    try { op(p) } finally { p.close() }
  }

  /** Create directory if it doesn't exist */
  def createDirectory(path: String) = {
    if (!Files.exists(Paths.get(path))) {
      val directory = new File(path)
      directory.mkdir()
    }
  }

  /**
   * Checks whether given directory exists and creates one if it doesn't
   */
  def checkOutputDirectory(outputDirectoryName: String) = {
    val outputDirectory = new File(outputDirectoryName)

    if (!outputDirectory.exists())
      createDirectory(outputDirectoryName)
  }

  /**
   * Because `file.delete()` doesn't delete a directory. Uses
   * [[java.nio.file.Files.walkFileTree]], and so doesn't follow symlinks.
   *
   * @throws IOException if something goes wrong.
   */
  def rmdir(path: Path) = {
    // Adapted from http://stackoverflow.com/questions/779519/delete-directories-recursively-in-java/8685959#8685959
    Files.walkFileTree(path, new SimpleFileVisitor[Path]() {
      override def visitFile(file: Path, attrs: BasicFileAttributes): FileVisitResult = {
        Files.delete(file)
        FileVisitResult.CONTINUE
      }

      override def visitFileFailed(file: Path, exc: IOException): FileVisitResult = {
        Files.delete(file)
        FileVisitResult.CONTINUE
      }

      override def postVisitDirectory(dir: Path, exc: IOException): FileVisitResult = {
        if (exc == null) {
          Files.delete(dir)
          FileVisitResult.CONTINUE
        } else {
          // directory iteration failed propagate exception
          throw exc
        }
      }
    })
  }
}
