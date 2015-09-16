package edu.nus.systemtesting

import scala.collection.mutable.ListBuffer
import scala.sys.process.{ stringToProcess, ProcessLogger }
import java.nio.file.Path

/**
 * @author richardg
 */
object ProgramFlags {
  // Not strictly true, but close enough for our purposes
  def isFlag(f: String) = f.startsWith("-")

  /**
   * List the flags of a `command`.
   * Assumes flags are printed, one on each line, beginning with a `-`,
   * and can be printed to standard out using `helpFlag = --help`.
   */
  def flagsOfProgram(command: Path, helpFlag: String = "--help") = {
    val flags = ListBuffer[String]()

    val collectFlags = ProcessLogger(line => {
      val parts = line.trim().split(" ")

      if (!parts.isEmpty) {
        val flag = parts(0)

        if (isFlag(flag))
          flags += flag
      }
    })

    val proc = (command + " " + helpFlag).run(collectFlags)
    proc.exitValue()

    flags
  }
}
