package edu.nus.systemtesting.hipsleek.app

import java.io.IOException
import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.Paths

import edu.nus.systemtesting.BinCache
import edu.nus.systemtesting.FileSystemUtilities
import edu.nus.systemtesting.hg.Commit
import edu.nus.systemtesting.hg.Repository
import edu.nus.systemtesting.hipsleek.BuildFailed
import edu.nus.systemtesting.hipsleek.BuildResult
import edu.nus.systemtesting.hipsleek.BuildTimedOut
import edu.nus.systemtesting.hipsleek.HipSleekPreparation
import edu.nus.systemtesting.hipsleek.SuccessfulBuildResult
import edu.nus.systemtesting.output.GlobalReporter.reporter

/**
 * @author richardg
 */
class RunUtils(config: AppConfig) {
  val binCache = new BinCache(config.binCacheDir)

  val repoDir: Path = config.repoDirOrDie

  // Each instance of `ConfiguredMain` only ever uses the one `Repository`
  val repo = new Repository(repoDir)


  private[app] def runTestsWith[T](revision: Commit, examplesDir: String)
                             (f: (Path, Path, Commit) => T):
      BuildResult[T] = {
    // check if bin cache has the binaries already
    binCache.binFor(Paths.get("hip"), revision) match {
      case Some(p) if !revision.isDirty => {
        val binDir = p getParent()
        // *May* be worth distinguishing "SuccessfulBuild" vs "Loaded Results"
        SuccessfulBuildResult(runTestsWithCached(binDir, revision, examplesDir)(f))
      }

      case None =>
        runTestsWithRepo(revision, examplesDir)(f)
    }
  }

  /**
   * `repoDir` is assumed to be a repository. This method creates an archive
   * of the repo in a tmp directory if the revision is specified, or
   * if the repository is 'clean'.
   *
   * If the repository is dirty, the repository's working directory is used
   * to run, and it is assumed that this folder can be used to make, and
   * run the tests in.
   */
  private def runTestsWithRepo[T](revision: Commit, examplesDir: String)
                                 (f: (Path, Path, Commit) => T):
      BuildResult[T] = {
    // Prepare the repo
    reporter.log("Preparing repo...")

    val isDirty = revision.isDirty

    withTmpDir("edunussystest") { tmpDir =>
      val projectDir = if (isDirty) {
        // i.e. LIVE, "in place"
        repoDir
      } else {
        val tmp = tmpDir.toAbsolutePath()

        // create archive of repo in tmp
        repo.archive(tmp, revision)

        tmp
      }

      val prep = new HipSleekPreparation(projectDir)
      val (prepResult, prepRemarks) = prep.prepare()

      prepRemarks.foreach(reporter.log)
      reporter.println()

      // Run the tests
      prepResult match {
        case SuccessfulBuildResult(()) => {
          val binDir = projectDir
          val corpusDir = projectDir resolve examplesDir

          // Copy to cache..
          // n.b. revision from repo.identify. (a type might help ensure it's 12 char long..)
          binCache.cache(binDir, Paths.get("sleek"), revision)
          binCache.cache(binDir, Paths.get("hip"), revision)
          // apparently prelude.ss needs to be in, or hip will break.
          binCache.cache(binDir, Paths.get("prelude.ss"), revision)

          SuccessfulBuildResult(f(binDir, corpusDir, revision))
        }
        case BuildTimedOut() => {
          BuildTimedOut()
        }
        case BuildFailed() => {
          BuildFailed()
        }
      }
    }
  }

  private def runTestsWithCached[T](binDir: Path, revision: Commit, examplesDir: String)
                                   (f: (Path, Path, Commit) => T): T = {
    // don't know whether it's hip/sleek we want, but we make/cache both, so.
    require((binDir resolve "sleek").toFile().exists())
    require((binDir resolve "hip").toFile().exists())

    reporter.log("Using cached binaries...")

    val isDirty = revision.isDirty

    // Need to have corpusDir; easiest to get from archive.
    // May be nice if could just "hg cat" (or so) over a bunch of files,
    //  might save time.
    withTmpDir("edunussystest") { tmpDir =>
      val projectDir = if (isDirty) {
        // i.e. LIVE, "in place",
        // esp. in case user makes use of example they modified/added
        repoDir
      } else {
        val tmp = tmpDir.toAbsolutePath()

        // Folders used by e.g. SleekTestSuiteUsage, HipTestSuiteUsage
        // TODO Hardcoded for now, due to architecture.
        val foldersUsed =  List(
          "examples/working/sleek",
          "examples/working/hip",
          "examples/working/hip_baga",
          "examples/working/infer",
          "examples/working/tree_shares",
          "examples/modular_examples"
        )

        // create archive of repo in tmp
        repo.archive(tmp, revision, foldersUsed)

        tmp
      }

      val corpusDir = projectDir resolve examplesDir

      f(binDir, corpusDir, revision)
    }
  }

  /**
   * @param f synchronous function; tmpdir is removed after f finishes
   */
  private def withTmpDir[T](name: String = "edunussystest",
                            removeAfterUse: Boolean = true)
                           (f: Path => T): T = {
    val tmpDir = Files createTempDirectory "edunussystest"

    val rtn = f(tmpDir)

    // Finished running the tests, clean up.
    try {
      if (removeAfterUse) {
        reporter.log("Deleting " + tmpDir)
        FileSystemUtilities rmdir tmpDir
      }
    } catch {
      case ioEx: IOException => {
        System.err.println(s"Unable to delete dir $tmpDir")
        ioEx.printStackTrace()
      }
    }

    rtn
  }
}
