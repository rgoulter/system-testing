package systemTestingDSL

import systemTestingDSL.outputGenerator.ConsoleOutputGenerator
import systemTestingDSL.matchers.DiffMatcher
import scala.collection.mutable.ArrayBuffer
import collection.JavaConversions._
import com.typesafe.config.Config

class ReferenceTestRunner(configuration: Config) extends ConsoleOutputGenerator {

  private def getFileList(directory: String, extension: String): Array[String] = {
    FileSystemUtilities.getRecursiveListOfFilesWithRegex(directory, extension).map(_.getAbsolutePath())
  }

  def run(): String = {
    val refTests = configuration.getConfigList("RUN_REFERENCE_TESTS")
    val referenceRuns = ArrayBuffer[GenericTestCase]()
    var diffOutput: String = ""
    for (configuration <- refTests) {
      val files = getFileList(configuration.getString("SOURCE_DIRECTORY"), configuration.getString("SOURCE_EXTENSION"))
      val referenceDirectory = configuration.getString("REF_DIRECTORY")
      val commandName = configuration.getString("COMMAND_NAME")
      val arguments = configuration.getString("ARGUMENTS")
      //      val referenceFileExtension = configuration.getString("REF_EXTENSION")
      val referenceFileExtension = ".ref"
      for (file <- files) {
        scala.util.control.Exception.ignoring(classOf[Exception]) {
          new GenericTestCase(commandName, file, arguments, referenceDirectory,
            file.substring(file.lastIndexOf("/") + 1), ".out").run
          diffOutput += file + "\n"
          diffOutput += "*************************\n"
          diffOutput += DiffMatcher.diff(referenceDirectory + file.substring(file.lastIndexOf("/") + 1) + ".out", referenceDirectory + file.substring(file.lastIndexOf("/") + 1) + ".ref")
        }
      }
    }
    println(diffOutput)
    diffOutput
  }
}
