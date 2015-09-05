package edu.nus.systemtesting.testsuite

import com.typesafe.config.Config

import edu.nus.systemtesting.FileSystemUtilities

/**
 * Used only for a helper function to write output to a file.
 * 'Generator' is a misnomer.
 */
trait PerformanceMetricsGenerator extends {
  def MILLI_CONVERSION_FACTOR = 1000

  def createPerformanceReport(performanceOutput : String,
                              configuration : Config,
                              writeToFile : (String, String, String, String) => Unit) : Unit = {
    val fileName = "sleek_performance_report_" + FileSystemUtilities.getCurrentDateString

    writeToFile(fileName, configuration.getString("SLEEK_OUTPUT_DIRECTORY"), performanceOutput, ".perf")
  }
}
