package edu.nus.systemtesting

import org.junit.Assert.assertEquals
import org.junit.Assert.assertTrue
import org.junit.Test

import edu.nus.systemtesting.HipTestCase.constructHipTestCase

class HipTestCaseTest {
  @Test
  def arr_sumTest() : Unit = {
    val arrSumTest = 
      (new TestCaseBuilder
        runCommand "hip"
        onFile "/home/rohit/hg/sleek_hip/examples/working/hip hip/array/arr_sum.ss"
        withArguments ""
        storeOutputInDirectory "results"
        withOutputFileName "arr_sum"
        checkAgainst "sigmaleft: FAIL, sigmaright: FAIL, test: SUCCESS")

    val execOut = new ExecutionOutput(HipTestCaseData.arr_sumOutput.split("\n"), Array(), 0)

    // Previously, was with regex
    // "Procedure.*FAIL.*|Procedure.*SUCCESS.*"

    val result = arrSumTest.generateTestResult(execOut, 200L)

    assertTrue(result.remarks.isEmpty)
    assertEquals(TestPassed, result.result)
  }
}
