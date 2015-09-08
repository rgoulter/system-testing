package edu.nus.systemtesting.hipsleek

import sys.process._
import edu.nus.systemtesting.SystemPreparation
import edu.nus.systemtesting.Runnable.executeProc
import java.io.File
import edu.nus.systemtesting.Runnable

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

  def existsOnPath(cmd : String) : Boolean = {
    // A program `cmd` is on path iff `which $cmd` returns 0
    val (whichOutp, _) = Runnable.execute(s"which $cmd")
    whichOutp.exitValue == 0
  }

  /** Returns provers *not* on the system. */
  def missingProvers() : Iterable[(String, String)] = {
    Provers.filterNot({
      case (name, cmd) => existsOnPath(cmd)
    })
  }
}

/**
 * Encapsulate preparation of Hip/Sleek repo.
 * In particular, ensures there's a natively-compiled version of
 * Hip/Sleek in the repo dir.
 * @author richardg
 */
class HipSleekPreparation(repoDir : String) extends SystemPreparation {
  def prepare() : (Boolean, Iterable[String]) = {
    val repoDirFile = new File(repoDir)

    // In order to `make native`, need to make the xml dep.
    val xmlDepDir = new File(repoDirFile, "xml")
    val mkXmlOutp = executeProc(Process("make", xmlDepDir))

    if (mkXmlOutp.exitValue != 0) {
      return (false,
              "Error building XML dep, err output:" +: mkXmlOutp.stderrLines.toList)
    }

    // make native
    // (takes about 3 minutes)
    val mkNativeOutp = executeProc(Process("make native", repoDirFile),
                                   timeout = 300)

    if (mkNativeOutp.exitValue != 0) {
      return (false,
              "Error building `make native`, err output:" +: mkNativeOutp.stderrLines.toList)
    }

    // Check for missing provers
    // which may affect whether hip/sleek can run.
    val warnings = HipSleekPreparation.missingProvers().map({
      case (name, cmd) => s"WARNING! Missing $name, expected to find `$cmd` on PATH!"
    })

    (true, warnings)
  }
}