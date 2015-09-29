package edu.nus.systemtesting.output

/** Enumeration of all the [[VisibilityOption]]s, which control the output. */
object VisibilityOptions extends Enumeration {
  // see http://www.scala-lang.org/api/current/index.html#scala.Enumeration

  type VisibilityOption = Value

  /** enable colourful output for console. */
  val ShowANSI = Value

  /** show results which are invalid. (may not be interesting). */
  val ShowInvalidResults = Value
  /** show results which are passing */
  val ShowPassingResults = Value
  /** show results which are failing */
  val ShowFailingResults = Value

  /** show the diff from a failing test case */
  val ShowFailingDiff = Value
  /** output the _reason_ for why a test case failed to run.
   * (e.g. output of the execution).
   */
  val ShowInvalidReasons = Value

  /** output execution time (if significant) */
  val ShowExecutionTime = Value

  /** whether to show a summary of results */
  val ShowSummary = Value
}

import VisibilityOptions._

/**
 * @author richardg
 */
case class OutputVisibility(
  val visibility: Map[VisibilityOption, Boolean] = Map(
    ShowANSI -> true,
    ShowInvalidResults -> true,
    ShowPassingResults -> true,
    ShowFailingResults -> true,
    ShowFailingDiff -> true,
    ShowInvalidReasons -> true,
    ShowExecutionTime -> true,
    ShowSummary -> true
  )
) {
  /**
   * Convenience method, to act like a control structure.
   * e.g.
   * {{{
   * outputVis when(ShowSummary) {
   *   print(summary)
   * }
   * }}}
   * @param key
   * @param f function for outputting to console.
   */
  def when(key: VisibilityOption)(f: => Unit): Unit = {
    if (visibility.getOrElse(key, false)) {
      f
    }
  }

  def show(key: VisibilityOption): Boolean =
    visibility.getOrElse(key, false)

  def copyWith(key: VisibilityOption, b: Boolean = true): OutputVisibility = {
    this.copy(visibility = visibility + (key -> b))
  }
}

object OutputVisibility {
  /**
   * Verbose prints 'everything'. Useful for when generating the results.
   */
  val PresetVerbose = new OutputVisibility(
    visibility = Map(
      ShowANSI -> true,
      ShowInvalidResults -> true,
      ShowPassingResults -> true,
      ShowFailingResults -> true,
      ShowFailingDiff -> true,
      ShowInvalidReasons -> true,
      ShowExecutionTime -> true,
      ShowSummary -> true
    )
  )

  /**
   * Verbose prints 'everything'. Useful for when running diff.
   */
  val PresetSummaryOnly = new OutputVisibility(
    visibility = Map(
      ShowANSI -> true,
      ShowInvalidResults -> false,
      ShowPassingResults -> false,
      ShowFailingResults -> false,
      ShowFailingDiff -> false,
      ShowInvalidReasons -> false,
      ShowExecutionTime -> false,
      ShowSummary -> true
    )
  )
}