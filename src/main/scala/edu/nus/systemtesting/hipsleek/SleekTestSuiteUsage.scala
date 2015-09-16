package edu.nus.systemtesting.hipsleek

import java.io.PrintWriter
import edu.nus.systemtesting.testsuite.TestSuite
import edu.nus.systemtesting.TestCase
import edu.nus.systemtesting.TestCaseBuilder
import java.nio.file.Path

class SleekTestSuiteUsage(sleekCommand: Path,
                          examplesDir: Path,
                          significantTime: Long,
                          timeout: Int,
                          revision: String) extends ConstructSleekTests {
  def test(cmd: Path,
           file: Path,
           args: String,
           expectedOutput: String): SleekTestCase =
    (new TestCaseBuilder
       runCommand cmd
       onFile file
       withArguments args
       checkAgainst expectedOutput
       timeoutAfter timeout)

  def run(): Unit = {
    val tests: List[TestCase] =
     (test (sleekCommand, examplesDir resolve "sleek.slk", " ", "Valid, Valid, Valid, Fail") +:

      test (sleekCommand, examplesDir resolve "cll-d.slk", " ", "Valid") +:

      test (sleekCommand, examplesDir resolve "label-basic.slk", " --dis-eps", "Fail, Valid, Valid, Fail") +:

      test (sleekCommand, examplesDir resolve "label-dll.slk", " --dis-eps", "Fail, Valid, Valid, Valid") +:

      test (sleekCommand, examplesDir resolve "sleek1.slk", " ", "Fail") +:

      test (sleekCommand, examplesDir resolve "sleek10.slk", " ", "Valid, Fail") +:

      test (sleekCommand, examplesDir resolve "sleek2.slk", " ", "Fail, Valid, Fail, Fail, Valid, Valid, Valid, Fail") +:

      test (sleekCommand, examplesDir resolve "sleek3.slk", " --elp", "Valid") +:

      test (sleekCommand, examplesDir resolve "sleek4.slk", " ", "Valid, Valid") +:

      test (sleekCommand, examplesDir resolve "sleek6.slk", " ", "Valid, Valid") +:

      test (sleekCommand, examplesDir resolve "sleek7.slk", "  --dis-lem-gen ", "Valid") +:

      test (sleekCommand, examplesDir resolve "sleek8.slk", "  --dis-lem-gen ", "Valid") +:

      test (sleekCommand, examplesDir resolve "sleek8.slk", "  --elp ", "Valid") +:

      test (sleekCommand, examplesDir resolve "sleek9.slk", "  --elp ", "Valid, Valid") +:

      test (sleekCommand, examplesDir resolve "sleek12-lend.slk", " ", "Valid, Fail, Valid") +:

      test (sleekCommand, examplesDir resolve "sleek13-lend.slk", " ", "Valid, Valid, Valid, Fail") +:

      test (sleekCommand, examplesDir resolve "lst-under1.slk", " --inv-test", "Valid, Fail") +:

      test (sleekCommand, examplesDir resolve "lst-under2.slk", " --inv-test", "Fail, Valid") +:

      test (sleekCommand, examplesDir resolve "ll-under1a.slk", "  --inv-test --baga-xpure ", "Valid, Valid") +:

      test (sleekCommand, examplesDir resolve "ll-under1b.slk", "  --inv-test --baga-xpure ", "Fail, Valid") +:

      test (sleekCommand, examplesDir resolve "ll-under1c.slk", "  --inv-test --baga-xpure ", "Valid, Fail") +:

      test (sleekCommand, examplesDir resolve "ll-under1d.slk", "  --inv-test --baga-xpure ", "Valid, Valid") +:

      test (sleekCommand, examplesDir resolve "ll-under1e.slk", "  --inv-test --baga-xpure ", "Fail, Fail") +:

      test (sleekCommand, examplesDir resolve "ll-under1f.slk", "  --inv-test --baga-xpure ", "Valid, Fail") +:

      test (sleekCommand, examplesDir resolve "baga-test-eps.slk", " --eps", "Fail, Fail, Valid, Valid, Fail, Valid, Valid, Fail, Fail, Valid, Fail, Fail, Valid, Valid, Valid") +:

      test (sleekCommand, examplesDir resolve "baga-test.slk", " ", "Fail, Fail, Valid, Valid, Fail, Valid, Valid, Fail, Fail, Valid, Fail, Fail, Valid, Valid, Valid") +:

      test (sleekCommand, examplesDir resolve "baga-test-2.slk", " --dis-baga-xpure --dis-eps", "Fail, Fail, Valid, Valid, Fail, Valid, Valid, Fail, Fail, Valid, Fail, Fail, Valid, Valid, Fail") +:

      test (sleekCommand, examplesDir resolve "baga-test-2.slk", " --baga-xpure", "Fail, Fail, Valid, Valid, Fail, Valid, Valid, Fail, Fail, Valid, Fail, Fail, Valid, Valid, Valid") +:

      test (sleekCommand, examplesDir resolve "symb-diff.slk", " ", "Valid, Valid, Valid") +:

      test (sleekCommand, examplesDir resolve "xpure3nodes.slk", "", "Valid, Valid") +:

      test (sleekCommand, examplesDir resolve "infer/app-inv.slk", " --inv --dis-eps", "Valid, Valid, Fail, Valid, Valid, Valid") +:

      test (sleekCommand, examplesDir resolve "infer/app-inv2.slk", " --inv --dis-eps", "Valid, Valid, Valid, Fail") +:

      test (sleekCommand, examplesDir resolve "infer/infer1.slk", " ", "Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Fail, Valid, Valid, Valid, Valid") +:

      test (sleekCommand, examplesDir resolve "infer/infer2.slk", " ", "Valid, Valid, Valid, Fail, Valid, Fail, Valid, Valid, Fail") +:

      test (sleekCommand, examplesDir resolve "infer/infer4.slk", " ", "Fail, Fail, Val") +:

      test (sleekCommand, examplesDir resolve "infer/infer5.slk", " ", "Valid, Valid, Fail, Valid") +:

      test (sleekCommand, examplesDir resolve "infer/infer5a.slk", " ", "Fail, Valid") +:

      test (sleekCommand, examplesDir resolve "infer/infer6.slk", " ", "Valid") +:

      test (sleekCommand, examplesDir resolve "infer/infer7.slk", " ", "Valid, Valid, Valid, Valid, Fail, Valid, Valid, Valid, Fail, Valid") +:

      test (sleekCommand, examplesDir resolve "infer/infer8.slk", " ", "Valid, Valid, Valid, Fail, Fail, Valid, Valid, Fail, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Fail, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Fail, Fail, Fail, Valid, Valid, Valid") +:

      test (sleekCommand, examplesDir resolve "infer/infer9.slk", " ", "Valid, Valid, Valid, Valid, Valid, Fail, Valid, Fail, Valid, Valid") +:

      test (sleekCommand, examplesDir resolve "infer/infer10.slk", " ", "Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Fail, Valid, Fail, Fail, Fail, Valid") +:

      test (sleekCommand, examplesDir resolve "infer/infer11.slk", " ", "Fail") +:

      test (sleekCommand, examplesDir resolve "infer/infer12.slk", " ", "Valid, Fail, Fail, Fail, Fail, Valid, Fail, Fail, Fail, Fail, Fail, Valid, Fail, Fail, Fail, Valid, Valid, Valid") +:

      test (sleekCommand, examplesDir resolve "infer/infer12.slk", " ", "Valid, Fail, Valid, Fail, Fail, Valid, Valid, Valid, Valid, Fail, Fail, Valid, Fail, Fail, Fail, Valid, Valid, Valid") +:

      test (sleekCommand, examplesDir resolve "infer/infer13.slk", " --sa-en-cont", "Valid, Valid, Valid, Valid, Valid") +:

      test (sleekCommand, examplesDir resolve "infer/infer14.slk", " --sa-en-pure-field", "Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Fail, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid") +:

      test (sleekCommand, examplesDir resolve "infer/infer15.slk", " ", "Valid, Valid, Valid, Valid, Valid, Valid, Valid") +:

      test (sleekCommand, examplesDir resolve "infer/infer16.slk", " ", "Valid, Valid, Valid, Valid, Valid, Valid") +:

      test (sleekCommand, examplesDir resolve "ann2.slk", "  --imm --en-imm-inv --etcsu1 ", "Valid, Valid, Valid, Fail, Valid, Valid, Fail, Fail, Valid, Valid, Valid, Valid, Valid, Fail, Fail, Valid, Fail, Valid, Fail, Fail, Valid, Valid, Valid, Fail, Fail") +:

      test (sleekCommand, examplesDir resolve "imm/imm1.slk", "  --imm --etcsu1 ", "Fail, Valid, Valid, Valid, Valid, Valid") +:

      test (sleekCommand, examplesDir resolve "imm/imm2.slk", "  --imm --etcsu1 ", "Fail, Valid, Fail, Valid, Fail") +:

      test (sleekCommand, examplesDir resolve "imm/imm3.slk", "  --imm --etcsu1 ", "Fail, Fail, Valid, Valid, Valid") +:

      test (sleekCommand, examplesDir resolve "imm/imm4.slk", "  --imm --etcsu1 ", "Valid, Fail") +:

      test (sleekCommand, examplesDir resolve "imm/imm-hard.slk", "  --imm --eps", "Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid") +:

      test (sleekCommand, examplesDir resolve "imm-field/sleek01.slk", "  --field-ann --etcsu1 ", "Valid, Valid, Valid, Fail, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Fail") +:

      test (sleekCommand, examplesDir resolve "imm-field/sleek02.slk", "  --field-ann --etcsu1 ", "Fail, Valid, Valid, Valid, Valid, Valid, Valid, Fail, Valid, Valid, Valid, Valid, Fail") +:

      test (sleekCommand, examplesDir resolve "imm-field/sleek03.slk", "  --field-ann --etcsu1 ", "Valid, Fail, Valid, Valid, Fail, Valid, Valid, Fail, Valid, Valid, Fail") +:

      test (sleekCommand, examplesDir resolve "eps.slk", "  --dis-imm ", "Valid") +:

      test (sleekCommand, examplesDir resolve "imm-field/sleek05.slk", "  --field-ann --etcsu1 ", "Valid, Fail, Fail, Fail, Fail, Fail, Valid, Valid, Val") +:

      test (sleekCommand, examplesDir resolve "classic/classic1.slk", " ", "Valid, Valid, Valid, Valid, Valid, Valid, Fail, Fail") +:

      test (sleekCommand, examplesDir resolve "classic/classic1.slk", "  --classic", "Fail, Valid, Valid, Valid, Fail, Valid, Fail, Fail") +:

      test (sleekCommand, examplesDir resolve "classic/classic1a.slk", " ", "Fail, Valid, Fail, Valid, Valid, Valid, Fail, Fail, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Fail") +:

      List[TestCase]())

    val suite = new TestSuite(tests, revision, significantTime)
    val suiteResult = suite.runAllTests
    suiteResult generateTestStatistics(new PrintWriter(System.out))
  }
}
