package reportingDSL
import scala.sys.process._

trait Execute {

  type Command = String
  def execute(command: Seq[String], cwd: String): Seq[String] = {
    sys.process.Process(command, new java.io.File(cwd)).!!.split("\n")
  }

  def executeInDirectory(command: Command, cwd: String): Seq[String] = {
    println(command, cwd)
    execute(command.split(" "), cwd)
  }
}
