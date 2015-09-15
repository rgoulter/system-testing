package edu.nus.systemtesting.hipsleek

import java.io.PrintWriter
import edu.nus.systemtesting.testsuite.TestSuite
import edu.nus.systemtesting.TestCase
import edu.nus.systemtesting.TestCaseBuilder

class SleekTestSuiteUsage(sleekCommand: String,
                          examplesDir: String,
                          significantTime: Long,
                          timeout: Int,
                          revision: String) extends ConstructSleekTests {
  def test(cmd: String,
           file: String,
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
     (test (sleekCommand, examplesDir + "sleek.slk", " ", "Valid, Valid, Valid, Fail") +:

      test (sleekCommand, examplesDir + "cll-d.slk", " ", "Valid") +:

      test (sleekCommand, examplesDir + "label-basic.slk", " --dis-eps", "Fail, Valid, Valid, Fail") +:

      test (sleekCommand, examplesDir + "label-dll.slk", " --dis-eps", "Fail, Valid, Valid, Valid") +:

      test (sleekCommand, examplesDir + "sleek1.slk", " ", "Fail") +:

      test (sleekCommand, examplesDir + "sleek10.slk", " ", "Valid, Fail") +:

      test (sleekCommand, examplesDir + "sleek2.slk", " ", "Fail, Valid, Fail, Fail, Valid, Valid, Valid, Fail") +:

      test (sleekCommand, examplesDir + "sleek3.slk", " --elp", "Valid") +:

      test (sleekCommand, examplesDir + "sleek4.slk", " ", "Valid, Valid") +:

      test (sleekCommand, examplesDir + "sleek6.slk", " ", "Valid, Valid") +:

      test (sleekCommand, examplesDir + "sleek7.slk", "  --dis-lem-gen ", "Valid") +:

      test (sleekCommand, examplesDir + "sleek8.slk", "  --dis-lem-gen ", "Valid") +:

      test (sleekCommand, examplesDir + "sleek8.slk", "  --elp ", "Valid") +:

      test (sleekCommand, examplesDir + "sleek9.slk", "  --elp ", "Valid, Valid") +:

      test (sleekCommand, examplesDir + "sleek12-lend.slk", " ", "Valid, Fail, Valid") +:

      test (sleekCommand, examplesDir + "sleek13-lend.slk", " ", "Valid, Valid, Valid, Fail") +:

      test (sleekCommand, examplesDir + "lst-under1.slk", " --inv-test", "Valid, Fail") +:

      test (sleekCommand, examplesDir + "lst-under2.slk", " --inv-test", "Fail, Valid") +:

      test (sleekCommand, examplesDir + "ll-under1a.slk", "  --inv-test --baga-xpure ", "Valid, Valid") +:

      test (sleekCommand, examplesDir + "ll-under1b.slk", "  --inv-test --baga-xpure ", "Fail, Valid") +:

      test (sleekCommand, examplesDir + "ll-under1c.slk", "  --inv-test --baga-xpure ", "Valid, Fail") +:

      test (sleekCommand, examplesDir + "ll-under1d.slk", "  --inv-test --baga-xpure ", "Valid, Valid") +:

      test (sleekCommand, examplesDir + "ll-under1e.slk", "  --inv-test --baga-xpure ", "Fail, Fail") +:

      test (sleekCommand, examplesDir + "ll-under1f.slk", "  --inv-test --baga-xpure ", "Valid, Fail") +:

      test (sleekCommand, examplesDir + "baga-test-eps.slk", " --eps", "Fail, Fail, Valid, Valid, Fail, Valid, Valid, Fail, Fail, Valid, Fail, Fail, Valid, Valid, Valid") +:

      test (sleekCommand, examplesDir + "baga-test.slk", " ", "Fail, Fail, Valid, Valid, Fail, Valid, Valid, Fail, Fail, Valid, Fail, Fail, Valid, Valid, Valid") +:

      test (sleekCommand, examplesDir + "baga-test-2.slk", " --dis-baga-xpure --dis-eps", "Fail, Fail, Valid, Valid, Fail, Valid, Valid, Fail, Fail, Valid, Fail, Fail, Valid, Valid, Fail") +:

      test (sleekCommand, examplesDir + "baga-test-2.slk", " --baga-xpure", "Fail, Fail, Valid, Valid, Fail, Valid, Valid, Fail, Fail, Valid, Fail, Fail, Valid, Valid, Valid") +:

      test (sleekCommand, examplesDir + "symb-diff.slk", " ", "Valid, Valid, Valid") +:

      test (sleekCommand, examplesDir + "xpure3nodes.slk", "", "Valid, Valid") +:

      test (sleekCommand, examplesDir + "infer/app-inv.slk", " --inv --dis-eps", "Valid, Valid, Fail, Valid, Valid, Valid") +:

      test (sleekCommand, examplesDir + "infer/app-inv2.slk", " --inv --dis-eps", "Valid, Valid, Valid, Fail") +:

      test (sleekCommand, examplesDir + "infer/infer1.slk", " ", "Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Fail, Valid, Valid, Valid, Valid") +:

      test (sleekCommand, examplesDir + "infer/infer2.slk", " ", "Valid, Valid, Valid, Fail, Valid, Fail, Valid, Valid, Fail") +:

      test (sleekCommand, examplesDir + "infer/infer4.slk", " ", "Fail, Fail, Val") +:

      test (sleekCommand, examplesDir + "infer/infer5.slk", " ", "Valid, Valid, Fail, Valid") +:

      test (sleekCommand, examplesDir + "infer/infer5a.slk", " ", "Fail, Valid") +:

      test (sleekCommand, examplesDir + "infer/infer6.slk", " ", "Valid") +:

      test (sleekCommand, examplesDir + "infer/infer7.slk", " ", "Valid, Valid, Valid, Valid, Fail, Valid, Valid, Valid, Fail, Valid") +:

      test (sleekCommand, examplesDir + "infer/infer8.slk", " ", "Valid, Valid, Valid, Fail, Fail, Valid, Valid, Fail, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Fail, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Fail, Fail, Fail, Valid, Valid, Valid") +:

      test (sleekCommand, examplesDir + "infer/infer9.slk", " ", "Valid, Valid, Valid, Valid, Valid, Fail, Valid, Fail, Valid, Valid") +:

      test (sleekCommand, examplesDir + "infer/infer10.slk", " ", "Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Fail, Valid, Fail, Fail, Fail, Valid") +:

      test (sleekCommand, examplesDir + "infer/infer11.slk", " ", "Fail") +:

      test (sleekCommand, examplesDir + "infer/infer12.slk", " ", "Valid, Fail, Fail, Fail, Fail, Valid, Fail, Fail, Fail, Fail, Fail, Valid, Fail, Fail, Fail, Valid, Valid, Valid") +:

      test (sleekCommand, examplesDir + "infer/infer12.slk", " ", "Valid, Fail, Valid, Fail, Fail, Valid, Valid, Valid, Valid, Fail, Fail, Valid, Fail, Fail, Fail, Valid, Valid, Valid") +:

      test (sleekCommand, examplesDir + "infer/infer13.slk", " --sa-en-cont", "Valid, Valid, Valid, Valid, Valid") +:

      test (sleekCommand, examplesDir + "infer/infer14.slk", " --sa-en-pure-field", "Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Fail, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid") +:

      test (sleekCommand, examplesDir + "infer/infer15.slk", " ", "Valid, Valid, Valid, Valid, Valid, Valid, Valid") +:

      test (sleekCommand, examplesDir + "infer/infer16.slk", " ", "Valid, Valid, Valid, Valid, Valid, Valid") +:

      test (sleekCommand, examplesDir + "ann2.slk", "  --imm --en-imm-inv --etcsu1 ", "Valid, Valid, Valid, Fail, Valid, Valid, Fail, Fail, Valid, Valid, Valid, Valid, Valid, Fail, Fail, Valid, Fail, Valid, Fail, Fail, Valid, Valid, Valid, Fail, Fail") +:

      test (sleekCommand, examplesDir + "imm/imm1.slk", "  --imm --etcsu1 ", "Fail, Valid, Valid, Valid, Valid, Valid") +:

      test (sleekCommand, examplesDir + "imm/imm2.slk", "  --imm --etcsu1 ", "Fail, Valid, Fail, Valid, Fail") +:

      test (sleekCommand, examplesDir + "imm/imm3.slk", "  --imm --etcsu1 ", "Fail, Fail, Valid, Valid, Valid") +:

      test (sleekCommand, examplesDir + "imm/imm4.slk", "  --imm --etcsu1 ", "Valid, Fail") +:

      test (sleekCommand, examplesDir + "imm/imm-hard.slk", "  --imm --eps", "Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid") +:

      test (sleekCommand, examplesDir + "imm-field/sleek01.slk", "  --field-ann --etcsu1 ", "Valid, Valid, Valid, Fail, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Fail") +:

      test (sleekCommand, examplesDir + "imm-field/sleek02.slk", "  --field-ann --etcsu1 ", "Fail, Valid, Valid, Valid, Valid, Valid, Valid, Fail, Valid, Valid, Valid, Valid, Fail") +:

      test (sleekCommand, examplesDir + "imm-field/sleek03.slk", "  --field-ann --etcsu1 ", "Valid, Fail, Valid, Valid, Fail, Valid, Valid, Fail, Valid, Valid, Fail") +:

      test (sleekCommand, examplesDir + "eps.slk", "  --dis-imm ", "Valid") +:

      test (sleekCommand, examplesDir + "imm-field/sleek05.slk", "  --field-ann --etcsu1 ", "Valid, Fail, Fail, Fail, Fail, Fail, Valid, Valid, Val") +:

      test (sleekCommand, examplesDir + "classic/classic1.slk", " ", "Valid, Valid, Valid, Valid, Valid, Valid, Fail, Fail") +:

      test (sleekCommand, examplesDir + "classic/classic1.slk", "  --classic", "Fail, Valid, Valid, Valid, Fail, Valid, Fail, Fail") +:

      test (sleekCommand, examplesDir + "classic/classic1a.slk", " ", "Fail, Valid, Fail, Valid, Valid, Valid, Fail, Fail, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Fail") +:

      List[TestCase]())

    val suite = new TestSuite(tests, revision, significantTime)
    val suiteResult = suite.runAllTests
    suiteResult generateTestStatistics(new PrintWriter(System.out))
  }
}
