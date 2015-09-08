package edu.nus.systemtesting.hipsleek

import java.io.PrintWriter
import com.typesafe.config.Config
import SleekTestCase.constructSleekTestCase
import edu.nus.systemtesting.testsuite.TestSuite
import edu.nus.systemtesting.TestCase
import edu.nus.systemtesting.TestCaseBuilder

class SleekTestSuiteUsage(configuration : Config) {
  val REPO_DIR = configuration.getString("REPO_DIR")
  val SLEEK_COMMAND = configuration.getString("SLEEK_COMMAND")
  val WORKING_DIR = configuration.getString("SLEEK_DIR")
  val OUTPUT_DIR = configuration.getString("SLEEK_OUTPUT_DIRECTORY")

  def test(cmd : String,
           file : String,
           args : String,
           outputDir : String,
           outputFile : String,
           expectedOutput : String) : SleekTestCase =
    (new TestCaseBuilder
       runCommand cmd
       onFile file
       withArguments args
       storeOutputInDirectory outputDir
       withOutputFileName outputFile
       checkAgainst expectedOutput)

  def run() : Unit = {
    val tests : List[TestCase] =
     (test (SLEEK_COMMAND, WORKING_DIR + "sleek.slk", " ", OUTPUT_DIR, "sleek", "Valid, Valid, Valid, Fail") +:

      test (SLEEK_COMMAND, WORKING_DIR + "cll-d.slk", " ", OUTPUT_DIR, "cll_d", "Valid") +:

      test (SLEEK_COMMAND, WORKING_DIR + "label-basic.slk", " --dis-eps", OUTPUT_DIR, "label_basic", "Fail, Valid, Valid, Fail") +:

      test (SLEEK_COMMAND, WORKING_DIR + "label-dll.slk", " --dis-eps", OUTPUT_DIR, "label_dll", "Fail, Valid, Valid, Valid") +:

      test (SLEEK_COMMAND, WORKING_DIR + "sleek1.slk", " ", OUTPUT_DIR, "sleek1", "Fail") +:

      test (SLEEK_COMMAND, WORKING_DIR + "sleek10.slk", " ", OUTPUT_DIR, "sleek10", "Valid, Fail") +:

      test (SLEEK_COMMAND, WORKING_DIR + "sleek2.slk", " ", OUTPUT_DIR, "sleek2", "Fail, Valid, Fail, Fail, Valid, Valid, Valid, Fail") +:

      test (SLEEK_COMMAND, WORKING_DIR + "sleek3.slk", " --elp", OUTPUT_DIR, "sleek3", "Valid") +:

      test (SLEEK_COMMAND, WORKING_DIR + "sleek4.slk", " ", OUTPUT_DIR, "sleek4", "Valid, Valid") +:

      test (SLEEK_COMMAND, WORKING_DIR + "sleek6.slk", " ", OUTPUT_DIR, "sleek6", "Valid, Valid") +:

      test (SLEEK_COMMAND, WORKING_DIR + "sleek7.slk", "  --dis-lem-gen ", OUTPUT_DIR, "sleek7", "Valid") +:

      test (SLEEK_COMMAND, WORKING_DIR + "sleek8.slk", "  --dis-lem-gen ", OUTPUT_DIR, "sleek8", "Valid") +:

      test (SLEEK_COMMAND, WORKING_DIR + "sleek8.slk", "  --elp ", OUTPUT_DIR, "sleek8", "Valid") +:

      test (SLEEK_COMMAND, WORKING_DIR + "sleek9.slk", "  --elp ", OUTPUT_DIR, "sleek9", "Valid, Valid") +:

      test (SLEEK_COMMAND, WORKING_DIR + "sleek12-lend.slk", " ", OUTPUT_DIR, "sleek12_lend", "Valid, Fail, Valid") +:

      test (SLEEK_COMMAND, WORKING_DIR + "sleek13-lend.slk", " ", OUTPUT_DIR, "sleek13_lend", "Valid, Valid, Valid, Fail") +:

      test (SLEEK_COMMAND, WORKING_DIR + "lst-under1.slk", " --inv-test", OUTPUT_DIR, "lst_under1", "Valid, Fail") +:

      test (SLEEK_COMMAND, WORKING_DIR + "lst-under2.slk", " --inv-test", OUTPUT_DIR, "lst_under2", "Fail, Valid") +:

      test (SLEEK_COMMAND, WORKING_DIR + "ll-under1a.slk", "  --inv-test --baga-xpure ", OUTPUT_DIR, "ll_under1a", "Valid, Valid") +:

      test (SLEEK_COMMAND, WORKING_DIR + "ll-under1b.slk", "  --inv-test --baga-xpure ", OUTPUT_DIR, "ll_under1b", "Fail, Valid") +:

      test (SLEEK_COMMAND, WORKING_DIR + "ll-under1c.slk", "  --inv-test --baga-xpure ", OUTPUT_DIR, "ll_under1c", "Valid, Fail") +:

      test (SLEEK_COMMAND, WORKING_DIR + "ll-under1d.slk", "  --inv-test --baga-xpure ", OUTPUT_DIR, "ll_under1d", "Valid, Valid") +:

      test (SLEEK_COMMAND, WORKING_DIR + "ll-under1e.slk", "  --inv-test --baga-xpure ", OUTPUT_DIR, "ll_under1e", "Fail, Fail") +:

      test (SLEEK_COMMAND, WORKING_DIR + "ll-under1f.slk", "  --inv-test --baga-xpure ", OUTPUT_DIR, "ll_under1f", "Valid, Fail") +:

      test (SLEEK_COMMAND, WORKING_DIR + "baga-test-eps.slk", " --eps", OUTPUT_DIR, "baga_test_eps", "Fail, Fail, Valid, Valid, Fail, Valid, Valid, Fail, Fail, Valid, Fail, Fail, Valid, Valid, Valid") +:

      test (SLEEK_COMMAND, WORKING_DIR + "baga-test.slk", " ", OUTPUT_DIR, "baga_test", "Fail, Fail, Valid, Valid, Fail, Valid, Valid, Fail, Fail, Valid, Fail, Fail, Valid, Valid, Valid") +:

      test (SLEEK_COMMAND, WORKING_DIR + "baga-test-2.slk", " --dis-baga-xpure --dis-eps", OUTPUT_DIR, "baga_test_2", "Fail, Fail, Valid, Valid, Fail, Valid, Valid, Fail, Fail, Valid, Fail, Fail, Valid, Valid, Fail") +:

      test (SLEEK_COMMAND, WORKING_DIR + "baga-test-2.slk", " --baga-xpure", OUTPUT_DIR, "baga_test_2", "Fail, Fail, Valid, Valid, Fail, Valid, Valid, Fail, Fail, Valid, Fail, Fail, Valid, Valid, Valid") +:

      test (SLEEK_COMMAND, WORKING_DIR + "symb-diff.slk", " ", OUTPUT_DIR, "symb_diff", "Valid, Valid, Valid") +:

      test (SLEEK_COMMAND, WORKING_DIR + "xpure3nodes.slk", "", OUTPUT_DIR, "xpure3nodes", "Valid, Valid") +:

      test (SLEEK_COMMAND, WORKING_DIR + "infer/app-inv.slk", " --inv --dis-eps", OUTPUT_DIR, "infer_app_inv", "Valid, Valid, Fail, Valid, Valid, Valid") +:

      test (SLEEK_COMMAND, WORKING_DIR + "infer/app-inv2.slk", " --inv --dis-eps", OUTPUT_DIR, "infer_app_inv2", "Valid, Valid, Valid, Fail") +:

      test (SLEEK_COMMAND, WORKING_DIR + "infer/infer1.slk", " ", OUTPUT_DIR, "infer_infer1", "Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Fail, Valid, Valid, Valid, Valid") +:

      test (SLEEK_COMMAND, WORKING_DIR + "infer/infer2.slk", " ", OUTPUT_DIR, "infer_infer2", "Valid, Valid, Valid, Fail, Valid, Fail, Valid, Valid, Fail") +:

      test (SLEEK_COMMAND, WORKING_DIR + "infer/infer4.slk", " ", OUTPUT_DIR, "infer_infer4", "Fail, Fail, Val") +:

      test (SLEEK_COMMAND, WORKING_DIR + "infer/infer5.slk", " ", OUTPUT_DIR, "infer_infer5", "Valid, Valid, Fail, Valid") +:

      test (SLEEK_COMMAND, WORKING_DIR + "infer/infer5a.slk", " ", OUTPUT_DIR, "infer_infer5a", "Fail, Valid") +:

      test (SLEEK_COMMAND, WORKING_DIR + "infer/infer6.slk", " ", OUTPUT_DIR, "infer_infer6", "Valid") +:

      test (SLEEK_COMMAND, WORKING_DIR + "infer/infer7.slk", " ", OUTPUT_DIR, "infer_infer7", "Valid, Valid, Valid, Valid, Fail, Valid, Valid, Valid, Fail, Valid") +:

      test (SLEEK_COMMAND, WORKING_DIR + "infer/infer8.slk", " ", OUTPUT_DIR, "infer_infer8", "Valid, Valid, Valid, Fail, Fail, Valid, Valid, Fail, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Fail, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Fail, Fail, Fail, Valid, Valid, Valid") +:

      test (SLEEK_COMMAND, WORKING_DIR + "infer/infer9.slk", " ", OUTPUT_DIR, "infer_infer9", "Valid, Valid, Valid, Valid, Valid, Fail, Valid, Fail, Valid, Valid") +:

      test (SLEEK_COMMAND, WORKING_DIR + "infer/infer10.slk", " ", OUTPUT_DIR, "infer_infer10", "Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Fail, Valid, Fail, Fail, Fail, Valid") +:

      test (SLEEK_COMMAND, WORKING_DIR + "infer/infer11.slk", " ", OUTPUT_DIR, "infer_infer11", "Fail") +:

      test (SLEEK_COMMAND, WORKING_DIR + "infer/infer12.slk", " ", OUTPUT_DIR, "infer_infer12", "Valid, Fail, Fail, Fail, Fail, Valid, Fail, Fail, Fail, Fail, Fail, Valid, Fail, Fail, Fail, Valid, Valid, Valid") +:

      test (SLEEK_COMMAND, WORKING_DIR + "infer/infer12.slk", " ", OUTPUT_DIR, "infer_infer12", "Valid, Fail, Valid, Fail, Fail, Valid, Valid, Valid, Valid, Fail, Fail, Valid, Fail, Fail, Fail, Valid, Valid, Valid") +:

      test (SLEEK_COMMAND, WORKING_DIR + "infer/infer13.slk", " --sa-en-cont", OUTPUT_DIR, "infer_infer13", "Valid, Valid, Valid, Valid, Valid") +:

      test (SLEEK_COMMAND, WORKING_DIR + "infer/infer14.slk", " --sa-en-pure-field", OUTPUT_DIR, "infer_infer14", "Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Fail, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid") +:

      test (SLEEK_COMMAND, WORKING_DIR + "infer/infer15.slk", " ", OUTPUT_DIR, "infer_infer15", "Valid, Valid, Valid, Valid, Valid, Valid, Valid") +:

      test (SLEEK_COMMAND, WORKING_DIR + "infer/infer16.slk", " ", OUTPUT_DIR, "infer_infer16", "Valid, Valid, Valid, Valid, Valid, Valid") +:

      test (SLEEK_COMMAND, WORKING_DIR + "ann2.slk", "  --imm --en-imm-inv --etcsu1 ", OUTPUT_DIR, "ann2", "Valid, Valid, Valid, Fail, Valid, Valid, Fail, Fail, Valid, Valid, Valid, Valid, Valid, Fail, Fail, Valid, Fail, Valid, Fail, Fail, Valid, Valid, Valid, Fail, Fail") +:

      test (SLEEK_COMMAND, WORKING_DIR + "imm/imm1.slk", "  --imm --etcsu1 ", OUTPUT_DIR, "imm_imm1", "Fail, Valid, Valid, Valid, Valid, Valid") +:

      test (SLEEK_COMMAND, WORKING_DIR + "imm/imm2.slk", "  --imm --etcsu1 ", OUTPUT_DIR, "imm_imm2", "Fail, Valid, Fail, Valid, Fail") +:

      test (SLEEK_COMMAND, WORKING_DIR + "imm/imm3.slk", "  --imm --etcsu1 ", OUTPUT_DIR, "imm_imm3", "Fail, Fail, Valid, Valid, Valid") +:

      test (SLEEK_COMMAND, WORKING_DIR + "imm/imm4.slk", "  --imm --etcsu1 ", OUTPUT_DIR, "imm_imm4", "Valid, Fail") +:

      test (SLEEK_COMMAND, WORKING_DIR + "imm/imm-hard.slk", "  --imm --eps", OUTPUT_DIR, "imm_imm_hard", "Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid") +:

      test (SLEEK_COMMAND, WORKING_DIR + "imm-field/sleek01.slk", "  --field-ann --etcsu1 ", OUTPUT_DIR, "imm_field_sleek01", "Valid, Valid, Valid, Fail, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Fail") +:

      test (SLEEK_COMMAND, WORKING_DIR + "imm-field/sleek02.slk", "  --field-ann --etcsu1 ", OUTPUT_DIR, "imm_field_sleek02", "Fail, Valid, Valid, Valid, Valid, Valid, Valid, Fail, Valid, Valid, Valid, Valid, Fail") +:

      test (SLEEK_COMMAND, WORKING_DIR + "imm-field/sleek03.slk", "  --field-ann --etcsu1 ", OUTPUT_DIR, "imm_field_sleek03", "Valid, Fail, Valid, Valid, Fail, Valid, Valid, Fail, Valid, Valid, Fail") +:

      test (SLEEK_COMMAND, WORKING_DIR + "eps.slk", "  --dis-imm ", OUTPUT_DIR, "eps", "Valid") +:

      test (SLEEK_COMMAND, WORKING_DIR + "imm-field/sleek05.slk", "  --field-ann --etcsu1 ", OUTPUT_DIR, "imm_field_sleek05", "Valid, Fail, Fail, Fail, Fail, Fail, Valid, Valid, Val") +:

      test (SLEEK_COMMAND, WORKING_DIR + "classic/classic1.slk", " ", OUTPUT_DIR, "classic_classic1", "Valid, Valid, Valid, Valid, Valid, Valid, Fail, Fail") +:

      test (SLEEK_COMMAND, WORKING_DIR + "classic/classic1.slk", "  --classic", OUTPUT_DIR, "classic_classic1", "Fail, Valid, Valid, Valid, Fail, Valid, Fail, Fail") +:

      test (SLEEK_COMMAND, WORKING_DIR + "classic/classic1a.slk", " ", OUTPUT_DIR, "classic_classic1a", "Fail, Valid, Fail, Valid, Valid, Valid, Fail, Fail, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Valid, Fail") +:

      List[TestCase]())

    val suite = new TestSuite(configuration, tests, Some(new HipSleekPreparation(REPO_DIR)))
    suite.runAllTests
    suite generateTestStatistics
  }
}
