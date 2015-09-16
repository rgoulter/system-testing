package edu.nus.systemtesting.output

object ReporterColors {
  // Colors should be able to be printed on ANSI console
  val ColorWhite   = "white"
  val ColorRed     = "red"
  val ColorGreen   = "green"
  val ColorCyan    = "cyan"
  val ColorMagenta = "purple"
}

/**
 * @author richardg
 */
trait Reporter {
  import ReporterColors._

  def print(message: String): Unit

  /** Mark up a message in some format for some color. */
  def inColor(color: String)(message: String): String

  def println(message: String = ""): Unit = {
    print(message)
    print("\n")
  }
  
  /** Ordinary message */
  def log(message: String): Unit =
    println(message)

  /** Message for things going wrong */
  def error(message: String): Unit =
    println(inColor(ColorRed)(message))

  def header(message: String, color: String = ColorWhite): Unit = {
    val NumCol = 30
    def output(m: String) = println(inColor(color)(m))

    println()
    output("*" * NumCol)
    output(message)
    output("*" * NumCol)
    println()
  }
}