package edu.nus.systemtesting.output

import java.io.PrintStream

/**
 * Outputs to some output stream, (STDOUT by default), without ANSI codes.
 * @author richardg
 */
class PlainReporter(outp: PrintStream = System.out) extends Reporter {
  import ReporterColors._

  def print(message: String): Unit =
    outp.print(message)

  def inColor(color: String)(message: String): String =
    message
}