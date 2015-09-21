package edu.nus.systemtesting.serialisation

import java.nio.file.{ Files, Path, Paths }
import scala.io.Source
import org.joda.time.format.ISODateTimeFormat

import edu.nus.systemtesting.FileSystemUtilities
import edu.nus.systemtesting.hg.Repository
import edu.nus.systemtesting.output.GlobalReporter
import edu.nus.systemtesting.testsuite.TestSuiteResult
import edu.nus.systemtesting.testsuite.TestSuiteComparison

import GlobalReporter.reporter

class ResultsArchive(val resultsDir: String = "results") {
  /**
   * Map of `(name, rev) => File`.
   * `rev` expected to be 'short'. (12 chars).
   */
  private lazy val resultFiles = {
    val filesInDir = Paths.get(resultsDir).toFile().listFiles()

    // cf. filenameForSuiteResult
    val ResultNameRegex = "(.*)-([0-9a-f]+)-(\\d+)-(\\d+)\\.json".r
    val resultTuples = filesInDir.map({ file =>
      file.getName() match {
        case ResultNameRegex(name, rev, date, time) =>
          Some((name, rev, date + time, file))
        case _ => None
      }
    }).flatten

    // Build map of (name, rev) => [(datetimeStr, file)]
    val resultsMap = resultTuples.map({case (n,r,_,_) =>
      (n,r)
    }).toSet[(String, String)].map({ case (n,r) =>
      val dtFilePairs = resultTuples.filter({ case (name, rev, _, _) =>
        name == n && rev == r
      }).map({ case (_, _, dt, f) => (dt, f) })

      (n,r) -> dtFilePairs
    }).toMap

    // Take the latest result
    // from (name,rev) => (datetimeStr, file) map
    resultsMap.map({ case (nameRev, ls) =>
      val (latestDateTime, file) = ls.max

      nameRev -> file
    })
  }

  def resultsFor(name: String, rev: String): Option[TestSuiteResult] = {
    resultFiles.get((name, rev)).map({ file =>
      val src = Source.fromFile(file)
      val content = src.mkString
      src.close()

      TestSuiteResultJson.load(content)
    }).flatten
  }

  /** In format of `$name-$revision-$datetime.json` */
  private def filenameForSuiteResult(suiteResult: TestSuiteResult, name: String): String = {
    import suiteResult.{ repoRevision, datetime }
    val datetimeStr = datetime.toString("yyyyMMdd-HHmmss")

    s"$name-${repoRevision}-${datetimeStr}.json"
  }

  /**
   * Note for `name` that [[TestSuiteResult]] isn't necessarily from one command
   * (`hip` or `sleek`). With the current design, convenient to consider results
   * as 'hip results' or 'sleek results', though.
   *
   * @param name e.g. `"hip"`, `"sleek"`
   */
  def saveTestSuiteResult(suiteResult: TestSuiteResult, name: String): Unit = {
    import suiteResult.{ repoRevision, datetime }

    FileSystemUtilities.checkOutputDirectory(resultsDir)
    val filename = filenameForSuiteResult(suiteResult, name)
    val dump = TestSuiteResultJson.dump(suiteResult)

    val path = Paths.get(resultsDir, filename)
    reporter.log(s"\nSaving results to $path\n")
    FileSystemUtilities.printToFile(path.toFile())(_.print(dump))
  }
}
