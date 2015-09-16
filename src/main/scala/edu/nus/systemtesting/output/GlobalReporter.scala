package edu.nus.systemtesting.output

/**
 * Provides global access to some [[Reporter]] object.
 * May be convenient to use like `import GlobalReporter.reporter` or so.
 * @author richardg
 */
object GlobalReporter {
  var reporter = new ANSIReporter()
}