package edu.nus.systemtesting.hipsleek

import com.typesafe.config.ConfigFactory
import com.typesafe.config.ConfigException

object ConfigDefaults {
  val DefaultTimeout = 300
  val DefaultSignificantTimeThreshold = 1
}

import ConfigDefaults._

case class AppConfig(repoDir: String,
                     rev: Option[String] = None,
                     command: String = "none",
                     timeout: Int = DefaultTimeout,
                     significantTimeThreshold: Int = DefaultSignificantTimeThreshold)

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
    val repoDir = configuration.getString("REPO_DIR")

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
        c.copy(command = "sleek") } text("  run sleek test cases")
    cmd("hip") action { (_, c) =>
        c.copy(command = "hip") } text("  run hip test cases")
    cmd("all") action { (_, c) =>
        c.copy(command = "all") } text("  run sleek and hip test cases")
    cmd("svcomp") action { (_, c) =>
        c.copy(command = "svcomp") } text("  run svcomp test cases")
  }
}
