package edu.nus.systemtesting.hipsleek

import com.typesafe.config.ConfigFactory
import com.typesafe.config.ConfigException

import ConfigDefaults._
import java.nio.file.{ Path, Paths }

object ConfigDefaults {
  val DefaultTimeout = 300
  val DefaultSignificantTimeThreshold = 1
}

case class AppConfig(repoDir: Path,
                     revs: List[String] = List(),
                     command: String = "none",
                     timeout: Int = DefaultTimeout,
                     significantTimeThreshold: Int = DefaultSignificantTimeThreshold) {
  def rev(): Option[String] = revs.headOption

  def rev1(): Option[String] = rev

  def rev2(): Option[String] =
    if (revs.length == 2) Some(revs(1)) else None
}

object AppConfig {
  /**
   * Use typesafe's `ConfigFactory` to load settings into this [[AppConfig]].
   *
   * Throws a [[com.typesafe.config.ConfigException]] if required keys in the
   * config (e.g. `REPO_DIR`) are missing.
   */
  def load(): AppConfig = {
    val configuration = ConfigFactory.load()

    // Compulsory configuration settings
    val repoDir = Paths.get(configuration.getString("REPO_DIR"))

    // Optional configuration settings
    // Use defaults if missing from `configuration`
    val timeout = try {
      configuration.getInt("TIMEOUT")
    } catch {
      case e: ConfigException.Missing => DefaultTimeout
    }

    val significantTimeThreshold = try {
      configuration.getInt("SIGNIFICANT_TIME_THRESHOLD")
    } catch {
      case e: ConfigException.Missing => DefaultSignificantTimeThreshold
    }

    AppConfig(repoDir = repoDir,
              timeout = timeout,
              significantTimeThreshold = significantTimeThreshold)
  }

  // Use scopt to parse command-line arguments
  val CommandLineOptionsParser = new scopt.OptionParser[AppConfig]("system-tests") {
    head("run-system-tests", "0.3.0-SNAPSHOT")
    help("help") text("prints this usage text")
    version("version")
    cmd("sleek") action { (_, c) =>
        c.copy(command = "sleek") } text("run sleek test cases") children(
          arg[String]("<revision>") optional() action { (x, c) =>
          c.copy(revs = List(x)) } text("optional revision of project to test against")
          )
    cmd("hip") action { (_, c) =>
        c.copy(command = "hip") } text("run hip test cases") children(
          arg[String]("<revision>") optional() action { (x, c) =>
          c.copy(revs = List(x)) } text("optional revision of project to test against")
          )
    cmd("all") action { (_, c) =>
        c.copy(command = "all") } text("run sleek and hip test cases") children(
          arg[String]("<revision>") optional() action { (x, c) =>
          c.copy(revs = List(x)) } text("optional revision of project to test against")
          )
    cmd("svcomp") action { (_, c) =>
        c.copy(command = "svcomp") } text("run svcomp test cases") children(
          arg[String]("<revision>") optional() action { (x, c) =>
          c.copy(revs = List(x)) } text("optional revision of project to test against")
          )
    cmd("diff") action { (_, c) =>
        c.copy(command = "diff") } text("diff the sleek/hip test results") children(
          arg[String]("<rev1 [rev2]>") optional() maxOccurs(2) action { (x, c) =>
          c.copy(revs = c.revs :+ x) } text("optional revisions of project to test against. (old, current).")
          )
  }
}
