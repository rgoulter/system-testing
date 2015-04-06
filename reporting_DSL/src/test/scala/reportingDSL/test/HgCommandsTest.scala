package reportingDSL.test

import org.junit.Test
import org.junit.Assert._
import reportingDSL._

class HgCommandsTest {
  @Test
  def pullTest(): Unit = {
    assertEquals(HgCommands.pull, "hg pull")
  }

  @Test
  def HgListAllBranchesTest(): Unit = {
    val expected = Seq("branch-1                       1:5d907041bd09",
      "branch-2                       0:31880e923d15 (inactive)")
    assertEquals(expected, HgApi.listAllBranches("/Users/rohitmukherjee/dev/repositories/play_area/repo/"))
  }

  @Test
  def hgLastCommitDateTest(): Unit = {
    assertEquals("1428299063", HgApi.getLastCommitDate("/Users/rohitmukherjee/dev/repositories/play_area/repo", "branch-1"))
  }

  @Test
  def hgGetCommitListTest(): Unit = {
    val expected = Seq(Commit("1", "5d907041bd0945f6dc5e2dcd58ebeb9891475bf9", "1428299063"),
      Commit("0", "31880e923d1573b5df995fdf1711ea4564b7362a", "1428298991"))
    assertEquals(expected, HgApi.getCommitList("/Users/rohitmukherjee/dev/repositories/play_area/repo"))
  }
}
