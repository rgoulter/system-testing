package edu.nus.systemtesting.hipsleek

import java.io.PrintWriter
import com.typesafe.config.Config
import edu.nus.systemtesting.testsuite.TestSuite
import edu.nus.systemtesting.TestCase
import edu.nus.systemtesting.TestCaseBuilder

class SleekTestSuiteUsage(configuration: Config) extends ConstructSleekTests {
  val REPO_DIR = configuration.getString("REPO_DIR")
  val SLEEK_COMMAND = configuration.getString("SLEEK_COMMAND")
  val WORKING_DIR = configuration.getString("SLEEK_DIR")
  val OUTPUT_DIR = configuration.getString("SLEEK_OUTPUT_DIRECTORY")

  def test(cmd: String,
           file: String,
           args: String,
           expectedOutput: String): SleekTestCase =
    (new TestCaseBuilder
       runCommand cmd
       onFile file
       withArguments args
       checkAgainst expectedOutput)

  def run(): Unit = {
    val tests: List[TestCase] =
     (test (SLEEK_COMMAND, WORKING_DIR + "sleek.slk", " ", "Valid, Valid, Valid, Fail") +:

      test (SLEEK_COMMAND, WORKING_DIR + "cll-d.slk", " ", "Valid") +:

      test (SLEEK_COMMAND, WORKING_DIR + "label-basic.slk", " --dis-eps", "Fail, Valid, Valid, Fail") +:

      test (SLEEK_COMMAND, WORKING_DIR + "label-dll.slk", " --dis-eps", "Fail, Valid, Valid, Valid") +:

      test (SLEEK_COMMAND, WORKING_DIR + "sleek1.slk", " ", "Fail") +:

      test (SLEEK_COMMAND, WORKING_DIR + "sleek10.slk", " ", "Valid, Fail") +:

      test (SLEEK_COMMAND, WORKING_DIR + "sleek2.slk", " ", "Fail, Valid, Fail, Fail, Valid, Valid, Valid, Fail") +:

      test (SLEEK_COMMAND, WORKING_DIR + "sleek3.slk", " --elp", "Valid") +:

      test (SLEEK_COMMAND, WORKING_DIR + "sleek4.slk", " ", "Valid, Valid") +:

      test (SLEEK_COMMAND, WORKING_DIR + "sleek6.slk", " ", "Valid, Valid") +:

      test (SLEEK_COMMAND, WORKING_DIR + "sleek7.slk", "  --dis-lem-gen ", "Valid") +:

      test (SLEEK_COMMAND, WORKING_DIR + "sleek8.slk", "  --dis-lem-gen ", "Valid") +:

      test (SLEEK_COMMAND, WORKING_DIR + "sleek8.slk", "  --elp ", "Valid") +:

      test (SLEEK_COMMAND, WORKING_DIR + "sleek9.slk", "  --elp ", "Valid, Valid") +:

      test (SLEEK_COMMAND, WORKING_DIR + "sleek12-lend.slk", " ", "Valid, Fail, Valid") +:

      test (SLEEK_COMMAND, WORKING_DIR + "sleek13-lend.slk", " ", "Valid, Valid, Valid, Fail") +:

      test (SLEEK_COMMAND, WORKING_DIR + "lst-under1.slk", " --inv-test", "Valid, Fail") +:

      test (SLEEK_COMMAND, WORKING_DIR + "lst-under2.slk", " --inv-test", "Fail, Valid") +:

      test (SLEEK_COMMAND, WORKING_DIR + "ll-under1a.slk", "  --inv-test --baga-xpure ", "Valid, Valid") +:

      test (SLEEK_COMMAND, WORKING_DIR + "ll-under1b.slk", "  --inv-test --baga-xpure ", "Fail, Valid") +:

      test (SLEEK_COMMAND, WORKING_DIR + "ll-under1c.slk", "  --inv-test --baga-xpure ", "Valid, Fail") +:

      test (SLEEK_COMMAND, WORKING_DIR + "ll-under1d.slk", "  --inv-test --baga-xpure ", "Valid, Valid") +:

      test (SLEEK_COMMAND, WORKING_DIR + "ll-under1e.slk", "  --inv-test --baga-xpure ", "Fail, Fail") +:

      test (SLEEK_COMMAND, WORKING_DIR + "ll-under1f.slk", "  --inv-test --baga-xpure ", "Valid, Fail") +:

      test (SLEEK_COMMAND, WORKING_DIR + "baga-test-eps.slk", " --eps", "Fail, Fail, Valid, Valid, Fail, Valid, Valid, Fail, Fail, Valid, Fail, Fail, Valid, Valid, Valid") +:

      test (SLEEK_COMMAND, WORKING_DIR + "baga-test.slk", " ", "Fail, Fail, Valid, Valid, Fail, Valid, Valid, Fail, Fail, Valid, Fail, Fail, Valid, Valid, Valid") +:

      test (SLEEK_COMMAND, WORKING_DIR + "baga-test-2.slk", " --dis-baga-xpure --dis-eps", "Fail, Fail, Valid, Valid, Fail, Valid, Valid, Fail, Fail, Valid, Fail, Fail, Valid, Valid, Fail") +:

      test (SLEEK_COMMAND, WORKING_DIR + "baga-test-2.slk", " --baga-xpure", "Fail, Fail, Valid, Valid, Fail, Valid, Valid, Fail, Fail, Valid, Fail, Fail, Valid, Valid, Valid") +:

      test (SLEEK_COMMAND, WORKING_DIR + "symb-diff.slk", " ", "Valid, Valid, Valid") +:

      test (SLEEK_COMMAND, WORKING_DIR + "xpure3nodes.slk", "", "Valid, Valid") +:

      test (SLEEK_COMMAND, WORKING_DIR + "infer/app-inv.slk", " --inv --dis-eps", "Valid, Valid, Fail, Valid, Valid, Valid") +:

      test (SLEEK_COMMAND, WORKING_DIR + "infer/app-inv2.slk", " --inv --dis-eps", "Valid, Valid, Valid, Fail") +:

      test (SLEEK_COMMAND, WORKING_DIR + "infer/infer1.slk", " ", "Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Fail, Valid, Valid, Valid, Valid") +:

      test (SLEEK_COMMAND, WORKING_DIR + "infer/infer2.slk", " ", "Valid, Valid, Valid, Fail, Valid, Fail, Valid, Valid, Fail") +:

      test (SLEEK_COMMAND, WORKING_DIR + "infer/infer4.slk", " ", "Fail, Fail, Val") +:

      test (SLEEK_COMMAND, WORKING_DIR + "infer/infer5.slk", " ", "Valid, Valid, Fail, Valid") +:

      test (SLEEK_COMMAND, WORKING_DIR + "infer/infer5a.slk", " ", "Fail, Valid") +:

      test (SLEEK_COMMAND, WORKING_DIR + "infer/infer6.slk", " ", "Valid") +:

      test (SLEEK_COMMAND, WORKING_DIR + "infer/infer7.slk", " ", "Valid, Valid, Valid, Valid, Fail, Valid, Valid, Valid, Fail, Valid") +:

      test (SLEEK_COMMAND, WORKING_DIR + "infer/infer8.slk", " ", "Valid, Valid, Valid, Fail, Fail, Valid, Valid, Fail, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Fail, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Fail, Fail, Fail, Valid, Valid, Valid") +:

      test (SLEEK_COMMAND, WORKING_DIR + "infer/infer9.slk", " ", "Valid, Valid, Valid, Valid, Valid, Fail, Valid, Fail, Valid, Valid") +:

      test (SLEEK_COMMAND, WORKING_DIR + "infer/infer10.slk", " ", "Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Fail, Valid, Fail, Fail, Fail, Valid") +:

      test (SLEEK_COMMAND, WORKING_DIR + "infer/infer11.slk", " ", "Fail") +:

      test (SLEEK_COMMAND, WORKING_DIR + "infer/infer12.slk", " ", "Valid, Fail, Fail, Fail, Fail, Valid, Fail, Fail, Fail, Fail, Fail, Valid, Fail, Fail, Fail, Valid, Valid, Valid") +:

      test (SLEEK_COMMAND, WORKING_DIR + "infer/infer12.slk", " ", "Valid, Fail, Valid, Fail, Fail, Valid, Valid, Valid, Valid, Fail, Fail, Valid, Fail, Fail, Fail, Valid, Valid, Valid") +:

      test (SLEEK_COMMAND, WORKING_DIR + "infer/infer13.slk", " --sa-en-cont", "Valid, Valid, Valid, Valid, Valid") +:

      test (SLEEK_COMMAND, WORKING_DIR + "infer/infer14.slk", " --sa-en-pure-field", "Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Fail, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid") +:

      test (SLEEK_COMMAND, WORKING_DIR + "infer/infer15.slk", " ", "Valid, Valid, Valid, Valid, Valid, Valid, Valid") +:

      test (SLEEK_COMMAND, WORKING_DIR + "infer/infer16.slk", " ", "Valid, Valid, Valid, Valid, Valid, Valid") +:

      test (SLEEK_COMMAND, WORKING_DIR + "ann2.slk", "  --imm --en-imm-inv --etcsu1 ", "Valid, Valid, Valid, Fail, Valid, Valid, Fail, Fail, Valid, Valid, Valid, Valid, Valid, Fail, Fail, Valid, Fail, Valid, Fail, Fail, Valid, Valid, Valid, Fail, Fail") +:

      test (SLEEK_COMMAND, WORKING_DIR + "imm/imm1.slk", "  --imm --etcsu1 ", "Fail, Valid, Valid, Valid, Valid, Valid") +:

      test (SLEEK_COMMAND, WORKING_DIR + "imm/imm2.slk", "  --imm --etcsu1 ", "Fail, Valid, Fail, Valid, Fail") +:

      test (SLEEK_COMMAND, WORKING_DIR + "imm/imm3.slk", "  --imm --etcsu1 ", "Fail, Fail, Valid, Valid, Valid") +:

      test (SLEEK_COMMAND, WORKING_DIR + "imm/imm4.slk", "  --imm --etcsu1 ", "Valid, Fail") +:

      test (SLEEK_COMMAND, WORKING_DIR + "imm/imm-hard.slk", "  --imm --eps", "Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid") +:

      test (SLEEK_COMMAND, WORKING_DIR + "imm-field/sleek01.slk", "  --field-ann --etcsu1 ", "Valid, Valid, Valid, Fail, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Fail") +:

      test (SLEEK_COMMAND, WORKING_DIR + "imm-field/sleek02.slk", "  --field-ann --etcsu1 ", "Fail, Valid, Valid, Valid, Valid, Valid, Valid, Fail, Valid, Valid, Valid, Valid, Fail") +:

      test (SLEEK_COMMAND, WORKING_DIR + "imm-field/sleek03.slk", "  --field-ann --etcsu1 ", "Valid, Fail, Valid, Valid, Fail, Valid, Valid, Fail, Valid, Valid, Fail") +:

      test (SLEEK_COMMAND, WORKING_DIR + "eps.slk", "  --dis-imm ", "Valid") +:

      test (SLEEK_COMMAND, WORKING_DIR + "imm-field/sleek05.slk", "  --field-ann --etcsu1 ", "Valid, Fail, Fail, Fail, Fail, Fail, Valid, Valid, Val") +:

      test (SLEEK_COMMAND, WORKING_DIR + "classic/classic1.slk", " ", "Valid, Valid, Valid, Valid, Valid, Valid, Fail, Fail") +:

      test (SLEEK_COMMAND, WORKING_DIR + "classic/classic1.slk", "  --classic", "Fail, Valid, Valid, Valid, Fail, Valid, Fail, Fail") +:

      test (SLEEK_COMMAND, WORKING_DIR + "classic/classic1a.slk", " ", "Fail, Valid, Fail, Valid, Valid, Valid, Fail, Fail, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Fail") +:

      List[TestCase]())

    val suite = new TestSuite(configuration, tests)
    val suiteResult = suite.runAllTests
    suiteResult generateTestStatistics(new PrintWriter(System.out))
  }
}
