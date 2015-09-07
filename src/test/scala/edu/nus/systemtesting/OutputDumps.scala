package edu.nus.systemtesting

import scala.io.Source

/**
 * @author richardg
 */
object OutputDumps {
  def loadResource(url : String) : String = {
    val resourceIS = OutputDumps.getClass().getClassLoader().getResourceAsStream(url)

    val source = Source.fromInputStream(resourceIS)
    val contentStr = source.mkString

    source.close()

    contentStr
  }

  lazy val SleekExResource = loadResource("outputdump/sleek.sleek.slk.soccf-plser2-05.7ac44bdb0dfd.txt")
}