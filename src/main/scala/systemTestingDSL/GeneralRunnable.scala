package systemTestingDSL

/**
 * Any command that needs to be executed on a system level can be done using GeneralRunnable
 * Results are returned as string from method execute
 */
case class GeneralRunnable(commandName: String,
  outputFileName: String = "",
  outputDirectory: String = ".",
  fileName: String = "", arguments: String = "",
  expectedOutput: String = "")
    extends Runnable {
  override def formCommand(): String = {
    commandName.concat(separator).concat(arguments).concat(separator).concat(fileName)
  }
}
