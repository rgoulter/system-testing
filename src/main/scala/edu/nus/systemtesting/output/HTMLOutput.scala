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

  def htmlOfPassingTCR(c: String, name: String, tcr: TestCaseResult): String = {
    val template = htmlSTG.getInstanceOf("tcrPassing"); 

    template.add("name", "TODO: cmd, args, filename")

    // XXX Output runtime, if larger than threshold..

    template.render()
  }

  def htmlOfFailingTCR(c: String, name: String, tcr: TestCaseResult): String = {
    val template = htmlSTG.getInstanceOf("tcrFailing"); 

    template.add("name", "TODO: cmd, args, filename")
    template.add("expected", ???)
    template.add("actual", ???)

    template.render()
  }

  def htmlOfInvalidTCR(c: String, name: String, tcr: TestCaseResult): String = {
    val template = htmlSTG.getInstanceOf("tcrInvalid"); 

    template.add("name", "TODO: cmd, args, filename")
    template.add("output", ???)

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
    "TODO: html of diff"
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

  def dumpRepoStatus(tip: Commit, branchStatuses: List[BranchStatus]): Unit = {
    // generate ToC from recent branches
    val branchesToC = "to be implemented: HTML of Table of Contents for branches<br/>\n"

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