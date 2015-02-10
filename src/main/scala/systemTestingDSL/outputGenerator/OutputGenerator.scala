package systemTestingDSL.outputGenerator

import java.io.File

import systemTestingDSL.fileSystemUtilities

trait OutputGenerator {
  def writeToFile(name: String, directory: String, content: String, extension: String = "") = {
    fileSystemUtilities.checkOutputDirectory(directory)
    fileSystemUtilities.printToFile(new File(directory.concat(File.separator).concat(name).concat(extension)))(_.print(content))
  }

}