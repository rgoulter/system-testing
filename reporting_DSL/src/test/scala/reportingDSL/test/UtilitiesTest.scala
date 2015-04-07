package reportingDSL.test

import org.junit.Test
import org.junit.Assert._

import reportingDSL.Utilities;
import reportingDSL._

class UtilitiesTest {
  @Test
  def checkLastCommitDateTest(): Unit = {
    assertTrue(Utilities.checkLastCommitDate(1401235200, 365))
    assertFalse(Utilities.checkLastCommitDate(1401235200, 100))
  }

  @Test
  def convertTimestampToStringTest(): Unit = {
    assertEquals("Wed May 28 08:00:00 SGT 2014", Utilities.convertTimestampToString(1401235200))
  }

  @Test
  def fileOrDirectoryExistsTest(): Unit = {
    assertTrue(Utilities.fileOrDirectoryExists("/Users/rohitmukherjee/dev/repositories/play_area/repo/"))
  }

}