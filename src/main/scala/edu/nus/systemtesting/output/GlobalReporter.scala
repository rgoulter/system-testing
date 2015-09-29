package edu.nus.systemtesting.output

/**
 * Provides global access to some [[Reporter]] object.
 * May be convenient to use like `import GlobalReporter.reporter` or so.
 * @author richardg
 */
object GlobalReporter {
  // Set as ANSI reporter in main;
  // easier to have only 'when' and not 'whenNot' in OutputVisibility.
  var reporter: Reporter = new PlainReporter()

  var visibility = OutputVisibility.PresetVerbose
}