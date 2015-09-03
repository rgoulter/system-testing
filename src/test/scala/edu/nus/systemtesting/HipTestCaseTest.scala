package edu.nus.systemtesting

import org.junit.Assert.assertEquals
import org.junit.Test

class HipTestCaseTest {
  @Test
  def arr_sumTest(): Unit = {
    val arrSumTest = new HipTestCase(
      new HipTestCaseBuilder runCommand "hip"
        onFile "/home/rohit/hg/sleek_hip/examples/working/hip hip/array/arr_sum.ss"
        withArguments ""
        storeOutputInDirectory "results"
        withOutputFileName "arr_sum"
        checkAgainst "sigmaleft: FAIL, sigmaright: FAIL, test: SUCCESS")

    arrSumTest.parse(HipTestCaseData.arr_sumOutput, "Procedure.*FAIL.*|Procedure.*SUCCESS.*")

    val result = arrSumTest.generateTestResult
    val (err, passOrFail, time) = result

    assertEquals(None, err)
    assertEquals("Passed", passOrFail)
  }
}