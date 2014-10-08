package systemTestingDSL

case class Test (commandName: String, currentWorkingDirectory: String = "." , fileName: String = "", arguments: String = "") extends Runnable {



}