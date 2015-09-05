package edu.nus.systemtesting

trait GetFileList {
  def getFileList(directory : String, extension : String) : Array[String] = {
    FileSystemUtilities.getRecursiveListOfFilesWithRegex(directory, extension).map(_.getAbsolutePath())
  }
}
