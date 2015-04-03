package systemTestingDSL.testSuite

import com.typesafe.config.Config
import systemTestingDSL.FileSystemUtilities

trait PerformanceMetricsGenerator extends {

  def MILLI_CONVERSION_FACTOR = 1000
  def createPerformanceReport(performanceOutput: String, configuration: Config, writeToFile: (String, String, String, String) => Unit): Unit = {
    val fileName: String = "sleek_performance_report_" + FileSystemUtilities.getCurrentDateString
    writeToFile(fileName, configuration.getString("SLEEK_OUTPUT_DIRECTORY"), performanceOutput, ".perf")
  }

}