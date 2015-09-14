package edu.nus.systemtesting

import scala.io.Source

/**
 * @author richardg
 */
object OutputDumps {
  def loadResource(url: String): String = {
    val resourceIS = OutputDumps.getClass().getClassLoader().getResourceAsStream(url)

    val source = Source.fromInputStream(resourceIS)
    val contentStr = source.mkString

    source.close()

    contentStr
  }

  // For `sleek examples/working/sleek/sleek.slk`,
  // expected output is "Valid, Valid, Valid, Fail"
  lazy val SleekExResource = loadResource("outputdump/sleek.sleek.slk.soccf-plser2-05.7ac44bdb0dfd.txt")

  // expected output is "remove: SUCCESS, append: SUCCESS"
  lazy val HipExResource = loadResource("outputdump/hip.--dsd_--en-inf.inflist.ss.soccf-plser2-05.7ac44bdb0dfd.txt")
}
