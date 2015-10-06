package edu.nus.systemtesting.serialisation

import java.io.File
import java.nio.file.{ Files, Path, Paths }
import scala.io.Source
import org.joda.time.format.ISODateTimeFormat
import edu.nus.systemtesting.FileSystemUtilities
import edu.nus.systemtesting.hg.Repository
import edu.nus.systemtesting.output.GlobalReporter
import edu.nus.systemtesting.testsuite.TestSuiteResult
import edu.nus.systemtesting.testsuite.TestSuiteComparison
import GlobalReporter.reporter
import edu.nus.systemtesting.TestCaseResult
import edu.nus.systemtesting.Testable

class ResultsArchive(val resultsDir: String = "results") {
  // Results stored become keyed by:
  //   revision, command, filename, args
  // Store at:
  //   $resultsDir/$revision/$command_filename_args.json

  private def tidyCommand(cmd: Path): String =
    // assume as either "sleek" or "hip",
    cmd toFile() getName()

  private def tidyFilename(fn: Path): String =
    // filename may contain the following non-alphanumerics:
    //   / . - _
    // and sometimes uppercase letters.
    // not all illegal, but maybe annoying
    fn toString() replaceAll("[-/._]+", "") toLowerCase()

  private def tidyArgs(args: String): String =
    // arguments may contain the following non-alphanumerics:
    //  - / . _
    // and be space-separated.
    args replaceAll("[-/._ ]+", "")

  /** In format of `$name-$revision-$datetime.json` */
  private def filenameForTCResult(repoRevision: String, tcResult: TestCaseResult): String = {
    import tcResult.{ command, filename, arguments }

    // cmd
    val cmd = tidyCommand(command)

    // cf. http://superuser.com/questions/358855/what-characters-are-safe-in-cross-platform-file-names-for-linux-windows-and-os
    // the best non-alphanumerics to use in filenames are:
    //   -.,;_

    val fn = tidyFilename(filename)

    val niceArgs = tidyArgs(arguments)

    s"$repoRevision/${cmd}_${fn}_${niceArgs}.json"
  }

  /**
   * Map of `(rev) => Map[(cmd, fn, args) => File]`.
   * `rev` expected to be 'short'. (12 chars).
   */
  private lazy val resultFiles = {
    FileSystemUtilities.checkOutputDirectory(resultsDir)
    val revDirs = Paths get(resultsDir) toFile() listFiles()

    // assume each of revDirs is a revision..

    revDirs map { revDir =>
      (revDir getName(), filesForRevisionDir(revDir))
    } toMap
  }

  private def filesForRevisionDir(revDir: File): Map[(String, String, String), File] = {
    // cf. filenameForTCResult
    //   $resultsDir/$revision/$command_filename_args.json

    val filesInDir = revDir.listFiles()

    val ResultNameRegex = "(.*)_([0-9a-z]+)_([0-9a-z]*)\\.json".r

    filesInDir flatMap { file =>
      file.getName() match {
        case ResultNameRegex(cmd, fn, args) =>
          Some(((cmd, fn, args), file))
        case _ => {
          System.err.println(s"WARNING: File not matching regex: ${file.getName()}")
          None
        }
      }
    } toMap
  }

  def resultsFor(repoRevision: String, cmd: String): List[TestCaseResult] = {
    (resultFiles get(repoRevision) toList) flatMap { revMap =>
      (revMap keys) filter { case (c, _, _) =>
        c == cmd
      } flatMap { k =>
        revMap get k
      }
    } flatMap { file =>
      // FileSystemUtilities readFromFile ??
      val src = Source fromFile file
      val content = src.mkString
      src.close()

      TestCaseResultJson.load(content)
    }
  }

  def resultFor(repoRevision: String, cmd: Path, filename: Path, args: String): Option[TestCaseResult] = {
    resultFiles get(repoRevision) flatMap { revMap =>
      revMap.get((tidyCommand(cmd), tidyFilename(filename), tidyArgs(args)))
    } flatMap { file =>
      // FileSystemUtilities readFromFile ??
      val src = Source.fromFile(file)
      val content = src.mkString
      src.close()

      TestCaseResultJson.load(content)
    }
  }

  def resultFor(repoRevision: String)(tc: Testable): Option[TestCaseResult] = {
    resultFor(repoRevision, tc.commandName, tc.fileName, tc.arguments)
  }

  /**
   * Return list of `(revision, test case results)` pairs for the given tcr.
   *
   * Of course, this is just the latest which have been recorded,
   * not necessarily all the results which could be computed.
   */
  def resultsFor(tc: Testable): List[(String, TestCaseResult)] = {
    // resultFiles :: Map of `(rev) => Map[(cmd, fn, args) => File]`.
    resultFiles.iterator flatMap { case (rev, m) =>
      // m is Map[(cmd, fn, args) => file]
      m.get((tc.commandName.toString, tc.fileName.toString, tc.arguments)) flatMap { file =>
        // FileSystemUtilities readFromFile ??
        val src = Source fromFile file
        val content = src.mkString
        src.close()

        TestCaseResultJson.load(content) map { tcr =>
          (rev, tcr)
        }
      }
    } toList
  }

  def saveTestCaseResult(repoRevision: String, tcResult: TestCaseResult): Unit = {
    val filename = filenameForTCResult(repoRevision, tcResult)
    val path = Paths.get(resultsDir, filename)

    // ensure the folder exists
    val parentDir = path.getParent().toFile()
    parentDir.mkdirs()

    val dump = TestCaseResultJson.dump(tcResult)

    reporter.log(s"Saving results to $path") // this *will* be excessive...
    FileSystemUtilities.printToFile(path.toFile())(_.print(dump))
  }
}
