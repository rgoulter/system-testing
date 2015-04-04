package hg

import org.junit.Test
import org.junit.Assert._

class HgCommandsTest {
  @Test
  def pullTest() = {
    assertEquals(HgCommands.pull, "hg pull")
  }
}