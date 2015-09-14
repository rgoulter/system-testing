package edu.nus.systemtesting.hipsleek

import sys.process._
import edu.nus.systemtesting.Runnable.executeProc
import java.io.File
import edu.nus.systemtesting.Runnable
import edu.nus.systemtesting.hg.Repository
import java.nio.file.Files

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
class HipSleekPreparation(repoDir: String, rev : Option[String]) {
  val repo = new Repository(repoDir)

  val revision = rev match {
    case Some(r) => r
    case None => repo.identify()
  }

  private val useTmp = rev match {
    case Some(r) => true
    // If repo is dirty, we can't use `hg archive` command,
    // must use Working Directory to make the executable(s).
    case None => !repo.isDirty()
  }

  // More complicated than that;
  // * given no revision = use working directory of repo.
  // * given revision, = make archive from that revision (assume non-dirty)

  private val tmpDir = Files.createTempDirectory("edunussystest")

  val projectDir = if (repo.isDirty()) {
    repoDir
  } else {
    tmpDir.toString()
  }

  def prepare(): (Boolean, Iterable[String]) = {
    val projectDirFile = new File(projectDir)

    // Copy archive (if necessary)
    if (useTmp) {
      // Repo isn't dirty, so can use archive command to ensure a clean build.
      repo.archive(tmpDir.toString(), rev)
    }

    // In order to `make native`, need to make the xml dep.
    val xmlDepDir = new File(projectDirFile, "xml")
    println(s"Calling 'make' in ${xmlDepDir.toPath().toString()}")
    val mkXmlOutp = executeProc(Process("make", xmlDepDir))

    if (mkXmlOutp.exitValue != 0) {
      return (false,
              "Error building XML dep, err output:" +: mkXmlOutp.stderrLines.toList)
    }

    // make native
    // (takes about 3 minutes)
    println(s"Calling 'make native' in ${projectDirFile.toPath().toString()}")
    val mkNativeOutp = executeProc(Process("make native", projectDirFile),
                                   timeout = 300)

    if (mkNativeOutp.exitValue != 0) {
      return (false,
              "Error building `make native`, err output:" +: mkNativeOutp.stderrLines.toList)
    }

    println("Built successfully!")

    // TODO: copy the built binaries to a tmp dir; use those binaries elsewhere.
    // At the moment, if running system test on dirty repo, will make use of
    // Working Directory to make, and then run the tests.

    // Check for missing provers
    // which may affect whether hip/sleek can run.
    val warnings = HipSleekPreparation.missingProvers().map({
      case (name, cmd) => s"WARNING! Missing $name, expected to find `$cmd` on PATH!"
    })

    (true, warnings)
  }

  def clean(): Unit = {
    tmpDir.toFile().delete()
  }
}
