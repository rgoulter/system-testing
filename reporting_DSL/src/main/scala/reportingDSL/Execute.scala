package reportingDSL
import scala.sys.process._

trait Execute {
  def execute(command: Seq[String], cwd: String): Seq[String] = {
    sys.process.Process(command, new java.io.File(cwd)).lines
  }
}