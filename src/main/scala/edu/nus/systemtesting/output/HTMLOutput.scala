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

  // escape HTML, based on:
  // http://stackoverflow.com/a/25228492/2488640
  def escapeHTML(s: String): String = {
    val out = new StringBuilder(Math.max(16, s.length()));

    for (c <- s) {
      if (c > 127 || c == '"' || c == '<' || c == '>' || c == '&') {
        out.append("&#");
        out.append(c.toInt.toString());
        out.append(';');
      } else {
        out.append(c);
      }
    }

    out.toString();
  }

  def conciseText(s: List[String]): String = {
    val TruncateTo = 5

    if (s.length > TruncateTo) {
      val short = s.take(TruncateTo)

      val template = htmlSTG.getInstanceOf("conciseText");

      template.add("short", short toArray)
      template.add("long", s toArray)
      template.add("count", s.length - TruncateTo)

      template.render()
    } else {
      val template = htmlSTG.getInstanceOf("lines");

      template.add("arr", s toArray)

      template.render()
    }
  }

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

    template.add("name", escapeHTML(strOfTCR(tcr)))

    // XXX Output runtime, if larger than threshold..

    template.render()
  }

  def htmlOfFailingTCR(c: String, name: String, tcr: TestCaseResult): String = {
    val template = htmlSTG.getInstanceOf("tcrFailing");

    val (expA, actA, keyA) = (tcr.diff.map { res =>
      (res.expected, res.actual, res.key)
    }) unzip3

    template.add("name", escapeHTML(strOfTCR(tcr)))
    template.add("expected", expA.toArray)
    template.add("actual", actA.toArray)
    template.add("key", keyA.toArray)

    template.render()
  }

  def htmlOfInvalidTCR(c: String, name: String, tcr: TestCaseResult): String = {
    val outputList = tcr.remarks.map { s => escapeHTML(s) }
    val outputStr = conciseText(outputList.toList)

    val template = htmlSTG.getInstanceOf("tcrInvalid");

    template.add("name", escapeHTML(strOfTCR(tcr)))
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
    val tcrs = tsr.results.map { tcr => htmlOfTestCaseResult(c, name, tcr) + "\n" } mkString

    val template = htmlSTG.getInstanceOf("tsr");

    template.add("title", s"Result for $name Suite at Revision $c")
    template.add("tcrsHTML", tcrs)
    template.add("summary", htmlOfTSRSummary(tsr))

    template.render()
  }

  def htmlOfDiff(tsCmp: TestSuiteComparison, bisectResults: List[(Testable, Commit)]): String = {
    val template = htmlSTG.getInstanceOf("tsrCmp");

    def strOfTCPair(tcPair: (TestCaseResult, TestCaseResult)): String = {
      val (oldTC, curTC) = tcPair
      strOfTCR(curTC)
    }

    // XXX: How to mix in bisect results here?

    template.add("name", escapeHTML(tsCmp.comparisonName))
    template.add("oldRev", tsCmp.oldRevision)
    template.add("curRev", tsCmp.curRevision)
    template.add("nowValid", (tsCmp.nowSuccessfullyRuns map strOfTCPair) toArray)
    template.add("nowInvalid", (tsCmp.usedToSuccessfullyRun map strOfTCPair) toArray)
    template.add("nowPasses", (tsCmp.nowPasses map strOfTCPair) toArray)
    template.add("nowFails", (tsCmp.usedToPass map { case (oldTC, curTC) =>
      val bisectResult = bisectResults.find({ case (tc, c) =>
        tc.commandName == curTC.commandName &&
        tc.fileName == curTC.fileName &&
        tc.arguments == curTC.arguments
      }).map({ case (tc, c) =>
        c.revHash
      }).getOrElse("?")

      strOfTCR(curTC) + " first failure at <b>" + bisectResult + "</b>"
    }) toArray)
    template.add("diffDiffs", (tsCmp.diffDiffs map strOfTCPair) toArray)
    template.add("nowSlower", (tsCmp.curSlower map strOfTCPair) toArray)
    template.add("nowQuicker", (tsCmp.curQuicker map strOfTCPair) toArray)

    template.render()
  }

  def htmlOfBranchStatus(branchStatus: BranchStatus): String = {
    val title = s"<h2>Branch ${branchStatus.branch.name}</h2>\n"

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
    println("Generating HTML...")

    // generate ToC from recent branches
    val branchesToC = htmlOfBranchesTable(branchStatuses)

    // generate + concatenate HTML for each branch status,
    val branchesContent = branchStatuses map { bs =>
      htmlOfBranchStatus(bs) + "<br>\n"
    } mkString

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

    println("Done.")
  }
}
