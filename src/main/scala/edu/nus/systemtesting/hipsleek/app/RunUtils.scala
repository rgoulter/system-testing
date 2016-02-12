package edu.nus.systemtesting.hipsleek.app

import java.io.IOException
import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.Paths

import scala.concurrent.Channel

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

object RunUtils {
  /**
   * @param f synchronous function; tmpdir is removed after f finishes
   */
  def withTmpDir[T](name: String = "edunussystest",
                    removeAfterUse: Boolean = true)
                   (f: Path => T): T = {
    val tmpDir = Files createTempDirectory name

    val rtnChannel: Channel[T] = new Channel()

    // Outer try-finally, in case f throws exception.
    try {
      rtnChannel write f(tmpDir)
    } finally {
      // Finished calling f (or it threw an exception),
      // important to clean up directory created.

      // Inner try-catch for dealing with file I/O of deleting tmpDir.
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
    }

    rtnChannel.read
  }

  def fromConfig(config: AppConfig): RunUtils = {
    val binCache = new BinCache(config.binCacheDir)
    val repoDir = config.repoDirOrDie
    val filesToCopy = List("hip", "sleek", "prelude.ss")
    val prepare = HipSleekPreparation.prepare(_)

    new RunUtils(repoDir, binCache, filesToCopy, prepare)
  }
}

/**
 * @param filesToCopy list of files to cache from repository after a successful build.
 * @author richardg
 */
class RunUtils(repoDir: Path,
               binCache: BinCache,
               filesToCopy: List[String] = List("hip", "sleek", "prelude.ss"),
               prepare: Path => (BuildResult[Unit], Iterable[String]) = HipSleekPreparation.prepare) // TODO Should probably be explicit about this assumption when used
    {
  val repo = new Repository(repoDir)

  private[app] def runTestsWith[T](revision: Commit, foldersUsed: List[String])
                             (f: (Path, Path, Commit) => T):
      BuildResult[T] = {
    // check if bin cache has the binaries already
    binCache.cachedDirFor(revision) match {
      case Some(binDir) if !revision.isDirty => {
        // *May* be worth distinguishing "SuccessfulBuild" vs "Loaded Results"
        SuccessfulBuildResult(runTestsWithCached(binDir, revision, foldersUsed)(f))
      }

      // since binaries aren't stored for dirty revisions,
      // if the repo is dirty, it's the same as if no binary is cached for it.
      case Some(p) if revision.isDirty =>
        runTestsWithRepo(revision)(f)

      case None =>
        runTestsWithRepo(revision)(f)
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
  private def runTestsWithRepo[T](revision: Commit)
                                 (f: (Path, Path, Commit) => T):
      BuildResult[T] = {
    // Prepare the repo
    reporter.log("Preparing repo...")

    val isDirty = revision.isDirty

    RunUtils.withTmpDir("edunussystest") { tmpDir =>
      val projectDir = if (isDirty) {
        // i.e. LIVE, "in place"
        repoDir
      } else {
        val tmp = tmpDir.toAbsolutePath()

        // create archive of repo in tmp
        repo.archive(tmp, revision)

        tmp
      }

      val (prepResult, prepRemarks) = prepare(projectDir)

      prepRemarks.foreach(reporter.log)
      reporter.println()

      // Run the tests
      prepResult match {
        case SuccessfulBuildResult(()) => {
          val binDir = projectDir

          // Copy to cache..
          filesToCopy.foreach({ fileToCopy =>
            binCache.cache(binDir, Paths.get(fileToCopy), revision)
          })

          SuccessfulBuildResult(f(binDir, projectDir, revision))
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

  private def runTestsWithCached[T](binDir: Path, revision: Commit, foldersUsed: List[String])
                                   (f: (Path, Path, Commit) => T): T = {
    reporter.log("Using cached binaries...")

    val isDirty = revision.isDirty

    // Need to have corpusDir; easiest to get from archive.
    // May be nice if could just "hg cat" (or so) over a bunch of files,
    //  might save time.
    RunUtils.withTmpDir("edunussystest") { tmpDir =>
      val projectDir = if (isDirty) {
        // XXX But we assume revision isn't dirty?
        // i.e. LIVE, "in place",
        // esp. in case user makes use of example they modified/added
        repoDir
      } else {
        val tmp = tmpDir.toAbsolutePath()

        // create archive of repo in tmp
        repo.archive(tmp, revision, foldersUsed)

        tmp
      }

      f(binDir, projectDir, revision)
    }
  }
}
