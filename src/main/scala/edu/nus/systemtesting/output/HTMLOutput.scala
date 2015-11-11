package edu.nus.systemtesting.output

import scala.beans.BeanProperty
import scala.io.Source
import org.stringtemplate.v4.ST
import org.stringtemplate.v4.STGroupString
import edu.nus.systemtesting.Testable
import edu.nus.systemtesting.TestCaseResult
import edu.nus.systemtesting.TestFailed
import edu.nus.systemtesting.TestPassed
import edu.nus.systemtesting.hg.Branch
import edu.nus.systemtesting.hg.Commit
import edu.nus.systemtesting.hipsleek.app.BranchStatus
import edu.nus.systemtesting.testsuite.TestSuiteComparison
import edu.nus.systemtesting.testsuite.TestSuiteResult
import java.io.PrintWriter

/**
 * @author richardg
 */
object HTMLOutput {
  val TemplateGroup = "RepoStatus.html.stg"
  val tgIS = getClass.getClassLoader.getResourceAsStream(TemplateGroup)
  assert (tgIS != null)

  val tgContent = Source.fromInputStream(tgIS).mkString
  val htmlSTG = new STGroupString(tgContent)

  //
  // TCR
  //

  def strOfTCR(tcr: TestCaseResult): String = {
    if (tcr.arguments.trim().isEmpty())
      tcr.command + " " + tcr.filename
    else
      tcr.command + " " + tcr.arguments + " " + tcr.filename
  }

  def htmlOfPassingTCR(c: String, name: String, tcr: TestCaseResult): String = {
    val template = htmlSTG.getInstanceOf("tcrPassing");

    template.add("name", strOfTCR(tcr))

    // XXX Output runtime, if larger than threshold..

    template.render()
  }

  def htmlOfFailingTCR(c: String, name: String, tcr: TestCaseResult): String = {
    val template = htmlSTG.getInstanceOf("tcrFailing");

    val (expA, actA) = (tcr.diff.map { res =>
      (res.expected, res.actual)
    }) unzip

    template.add("name", strOfTCR(tcr))
    template.add("expected", expA.toArray)
    template.add("actual", actA.toArray)

    template.render()
  }

  def htmlOfInvalidTCR(c: String, name: String, tcr: TestCaseResult): String = {
    val template = htmlSTG.getInstanceOf("tcrInvalid");

    template.add("name", strOfTCR(tcr))

    // ENHANCEMENT: be able to 'truncate' output,
    //  if the output is excessive.
    val outputStr = tcr.remarks.map { s => s + "<br/>\n" } mkString

    template.add("output", outputStr)

    template.render()
  }

  def htmlOfTestCaseResult(c: String, name: String, tcr: TestCaseResult): String = {
    tcr.result match {
      case TestPassed => htmlOfPassingTCR(c, name, tcr)
      case TestFailed if tcr.executionSucceeded => htmlOfFailingTCR(c, name, tcr)
      case TestFailed if !tcr.executionSucceeded => htmlOfInvalidTCR(c, name, tcr)
    }
  }

  def htmlOfTSRSummary(tsr: TestSuiteResult): String = {
    val template = htmlSTG.getInstanceOf("tsrSummary");

    template.add("totalCt", tsr.results.length)
    template.add("passedCt", tsr.successes.length)
    template.add("failedCt", tsr.failures.length)
    template.add("invalidCt", tsr.invalid.length)

    template.render()
  }

  def htmlOfTestSuiteResult(c: String, name: String, tsr: TestSuiteResult): String = {
    // TCR results ~ TCR.display
    // Summary
    // TSRAnalysis Tally

    val title = s"TSR for $name at Revision $c<br/>\n"

    val tcrs = tsr.results.map { tcr => htmlOfTestCaseResult(c, name, tcr) + "\n" } mkString

    val summary = htmlOfTSRSummary(tsr) + "\n"

    // TODO: TSR Tally

    title + tcrs + summary
  }

  def htmlOfDiff(tsCmp: TestSuiteComparison, bisectResults: List[(Testable, Commit)]): String = {
    val template = htmlSTG.getInstanceOf("tsrCmp");

    // copied from TSCmp class; for 'consistency'.
    def tcrToString(tcr: TestCaseResult): String = {
      import tcr.{ command, arguments, filename }
      s"TC[$command, $arguments, $filename]"
    }

    def strOfTCPair(tcPair: (TestCaseResult, TestCaseResult)): String = {
      val (oldTC, curTC) = tcPair
      tcrToString(curTC)
    }

    template.add("name", tsCmp.comparisonName)
    template.add("oldRev", tsCmp.oldRevision)
    template.add("curRev", tsCmp.curRevision)
    template.add("nowValid", (tsCmp.nowSuccessfullyRuns map strOfTCPair) toArray)
    template.add("nowInvalid", (tsCmp.usedToSuccessfullyRun map strOfTCPair) toArray)
    template.add("nowPasses", (tsCmp.nowPasses map strOfTCPair) toArray)
    template.add("nowFails", (tsCmp.usedToPass map strOfTCPair) toArray)
    template.add("diffDiffs", (tsCmp.diffDiffs map strOfTCPair) toArray)
    template.add("nowSlower", (tsCmp.curSlower map strOfTCPair) toArray)
    template.add("nowQuicker", (tsCmp.curQuicker map strOfTCPair) toArray)

    template.render()
  }

  def htmlOfBranchStatus(branchStatus: BranchStatus): String = {
    val title = s"Branch ${branchStatus.branch.name}<br/>\n"

    // per TSCmp: ["hip", "sleek"]
    //   Old Commit TSR...
    //   New Commit TSR...
    //   Diff+Bisect Results

    val content = branchStatus.tsCmp map { diff =>
      import diff.{ oldRevision, oldTSR }
      import diff.{ curRevision, curTSR }

      val diffName = diff.comparisonName

      val oldTS = htmlOfTestSuiteResult(oldRevision, diffName, oldTSR)
      val curTS = htmlOfTestSuiteResult(curRevision, diffName, curTSR)

      val diffH = htmlOfDiff(diff, branchStatus.bisectResults)

      oldTS + curTS + diffH + "\n\n"
    } mkString

    title + content
  }

  def htmlOfBranchesTable(branchStatuses: List[BranchStatus]): String = {
    val template = htmlSTG.getInstanceOf("branchesToC");

    val branches = branchStatuses.map (_.branch)

    val names = branches map(_.name)
    val revs = branches map(_.revHash)
    val ages = branches map(_.age)
    val branchedFroms = branches map { br =>
      br.branchedFrom.map { c =>
        val branch = c.branch
        val bfName = branch.name
        val bfRev = branch.revHash
        val bfAge = branch.age

        s"$bfName $bfRev $bfAge"
      } getOrElse "-"
    }

    template.add("names", names toArray)
    template.add("revs", revs toArray)
    template.add("ages", ages toArray)
    template.add("branchedFroms", branchedFroms toArray)

    template.render()
  }

  def dumpRepoStatus(tip: Commit, branchStatuses: List[BranchStatus]): Unit = {
    // generate ToC from recent branches
    val branchesToC = htmlOfBranchesTable(branchStatuses)

    // generate + concatenate HTML for each branch status,
    val branchesContent = branchStatuses map { bs =>
      htmlOfBranchStatus(bs) + "<br>\n"
    }

    val htmlContent = branchesToC + branchesContent

    val template = htmlSTG.getInstanceOf("page");

    template.add("content", htmlContent)

    val repoStatusHTML = template.render()

    // dump to file
    val filename = "output.html" // XXX name as a function of the tip commit.

    val pw = new PrintWriter(filename)
    pw.println(repoStatusHTML)
    pw.flush()
    pw.close()
  }
}
