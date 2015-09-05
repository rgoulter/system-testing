package edu.nus.systemtesting

import java.io.File
import java.io.FileNotFoundException
import java.nio.file.Files
import java.nio.file.Paths
import java.text.SimpleDateFormat
import java.util.Date

/**
 * This object provides some convenient methods/functions to provide fileSystem
 * related operations
 */
object FileSystemUtilities {
  /**
   * Convenience Method to write data to a file
   */
  def printToFile(f : File)(op : java.io.PrintWriter => Unit) {
    val p = new java.io.PrintWriter(f)
    try { op(p) } finally { p.close() }
  }

  /**
   * Returns an array containing all files starting from parent directory and
   * traversing downwards
   */
  def getRecursiveListOfFiles(dir : File) : Array[File] = {
    val these = dir.listFiles
    these ++ these.filter(_.isDirectory).flatMap(getRecursiveListOfFiles)
  }

  /** Returns an array of files matching provided Regex and parent directory */
  def getRecursiveListOfFilesWithRegex(dir : File, regex : String) : Array[File] = {
    val files = getRecursiveListOfFiles(dir)
    files.filter(file => file.getName().endsWith(regex))
  }

  def getRecursiveListOfFilesWithRegex(directoryName : String, regex : String) : Array[File] = {
    getRecursiveListOfFilesWithRegex(new File(directoryName), regex)
  }

  /** Check if file or folder exists */
  def fileOrDirectoryExists(path : String) = Files.exists(Paths.get(path))

  /** Create directory if it doesn't exist */
  def createDirectory(path : String) = {
    if (!fileOrDirectoryExists(path)) {
      val directory = new File(path)
      directory.mkdir()
    }
  }

  /**
   * Reads file content as string and returns it
   */
  def readFile(path : String) : String = {
    var lines = ""

    try {
      val source = scala.io.Source.fromFile(path)
      lines = source.getLines mkString NEW_LINE
      source.close()
    } catch {
      case ex : FileNotFoundException =>
    }

    lines
  }

  /**
   * Checks whether given directory exists and creates one if it doesn't
   */
  def checkOutputDirectory(outputDirectoryName : String) = {
    val outputDirectory = new File(outputDirectoryName)

    if (!outputDirectory.exists())
      createDirectory(outputDirectoryName)
  }

  /**
   * Get Current date as string
   */
  def getCurrentDateString() : String = {
    val formatter = new SimpleDateFormat("dd_mm_yy")
    formatter.format(new Date())
  }
}
