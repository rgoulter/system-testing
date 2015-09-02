package systemTestingDSL.outputGenerator

import java.io.File

import systemTestingDSL.FileSystemUtilities

trait OutputGenerator {
  def writeToFile(name: String, directory: String, content: String, extension: String = ".out") = {
    FileSystemUtilities.checkOutputDirectory(directory)
    
    val filename = directory.concat(File.separator).concat(name).concat(extension)
    FileSystemUtilities.printToFile(new File(filename))(_.print(content))
  }

  def passed = success("Passed")

  def failed = error("Failed")

  def had(hadResult: String): String = actual(s"Had: $hadResult")

  def expected(expectedResult: String): String = expect(s"Expected: $expectedResult")

  def error(errorText: String): String

  def actual(errorText: String): String

  def expect(errorText: String): String

  def success(successText: String): String

  def log(logText: String): String
}