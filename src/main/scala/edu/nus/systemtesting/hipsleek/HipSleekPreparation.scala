package edu.nus.systemtesting.hipsleek

import sys.process._
import java.io.File
import edu.nus.systemtesting.Runnable
import edu.nus.systemtesting.hg.Repository
import java.nio.file.Files
import edu.nus.systemtesting.Runnable.executeProc
import edu.nus.systemtesting.output.GlobalReporter
import GlobalReporter.reporter
import java.nio.file.Path

sealed abstract class BuildResult[T]

case class SuccessfulBuildResult[T](val res: T) extends BuildResult[T]

case class BuildTimedOut[T]() extends BuildResult[T]

case class BuildFailed[T]() extends BuildResult[T]



object HipSleekPreparation {
  /**
   * Map of Prover name -> prover executable name.
   * It's expected that the executable name is on the `PATH`.
   */
  val Provers = Map(
    "Omega" -> "oc",
    "Z3" -> "z3",
    "Redlog" -> "redcsl",
    "Fixcalc" -> "fixcalc"
  )

  def existsOnPath(cmd: String): Boolean = {
    // A program `cmd` is on path iff `which $cmd` returns 0
    val (whichOutp, _) = Runnable.execute(s"which $cmd")
    whichOutp.exitValue == 0
  }

  /** Returns provers *not* on the system. */
  def missingProvers(): Iterable[(String, String)] = {
    Provers.filterNot({
      case (name, cmd) => existsOnPath(cmd)
    })
  }
}

/**
 * Encapsulate preparation of Hip/Sleek repo.
 * In particular, ensures there's a natively-compiled version of
 * Hip/Sleek in the repo dir.
 *
 * n.b. if no revision is given, and `repoDir` points to a dirty repository,
 * [[HipSleekPreparation]] will make the executables in place.
 * @author richardg
 */
class HipSleekPreparation(val projectDir: Path) {
  def prepare(): (BuildResult[Unit], Iterable[String]) = {
    // In order to `make native`, need to make the xml dep.
    val xmlDepDir = projectDir resolve "xml"

    reporter.log(s"Calling 'make' in ${xmlDepDir.toString()}")
    val mkXmlOutp = executeProc(Process("make", xmlDepDir toFile))

    if (mkXmlOutp.exitValue != 0) {
      // It's quite unlikely that the xml part will time out.
      // (so no need to treat timeout distinctly)
      return (BuildFailed(),
              "Error building XML dep, err output:" +: mkXmlOutp.stderrLines.toList)
    }

    // make native
    // (takes about 3 minutes)
    reporter.log(s"Calling 'make native' in ${projectDir.toString()}")
    // XXX note that this may timeout...
    val mkNativeOutp = executeProc(Process("make native", projectDir toFile),
                                   timeout = 300)

    if (mkNativeOutp.exitValue != 0) {
      if (mkNativeOutp.timedOut) {
        return (BuildTimedOut(),
                List("Error building `make native`, timed out."))
      } else {
        return (BuildFailed(),
                "Error building `make native`, err output:" +: mkNativeOutp.stderrLines.toList)
      }
    }

    reporter.log("Built successfully!")

    // TODO: copy the built binaries to a tmp dir; use those binaries elsewhere.
    // At the moment, if running system test on dirty repo, will make use of
    // Working Directory to make, and then run the tests.

    // Check for missing provers
    // which may affect whether hip/sleek can run.
    val warnings = HipSleekPreparation.missingProvers().map({
      case (name, cmd) => s"WARNING! Missing $name, expected to find `$cmd` on PATH!"
    })

    (SuccessfulBuildResult(()), warnings)
  }
}
