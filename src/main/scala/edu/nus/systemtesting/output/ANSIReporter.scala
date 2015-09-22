package edu.nus.systemtesting.output

import java.io.PrintStream

/**
 * Outputs ANSI-colored text to some output stream. (STDOUT by default).
 * @author richardg
 */
class ANSIReporter(outp: PrintStream = System.out) extends Reporter {
  import ReporterColors._

  def print(message: String): Unit =
    outp.print(message)

  def inColor(color: String)(message: String): String = {
    val ansi = color match {
      case ColorGreen   => Console.GREEN
      case ColorRed     => Console.RED
      case ColorCyan    => Console.CYAN
      case ColorMagenta => Console.MAGENTA
      case ColorWhite   => Console.WHITE
      case ColorYellow  => Console.YELLOW

      // for whatever, unknown color, use BOLD.
      case _ => Console.BOLD
    }

    s"$ansi$message${Console.RESET}"
  }
}