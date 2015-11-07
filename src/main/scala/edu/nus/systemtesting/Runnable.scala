package edu.nus.systemtesting

import java.io.FileNotFoundException
import java.util.concurrent.TimeoutException
import scala.concurrent.{ Await, Future }
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.DurationInt
import scala.sys.process.{ stringToProcess, ProcessLogger }
import scala.collection.mutable.ArrayBuffer
import scala.sys.process.ProcessBuilder

class ExecutionOutput(val stdoutLines: Array[String],
                      val stderrLines: Array[String],
                      val exitValue: Int,
                      val timedOut: Boolean = false) {
  /** `STDOUT` output from the execution. */
  def output = stdoutLines.mkString("\n")

  /** `STDERR` output from the execution. */
  def errOutput = stderrLines.mkString("\n")
}

object ExecutionOutput {
  /**
   * Construct output as if given string was the stdout of some execution.
   * Assumes return code 0, no stderr output.
   * For convenience/testing.
   */
  def outputFromString(out: String) = {
    new ExecutionOutput(out.split("\n"), Array(), 0)
  }
}

/**
 * This object provides methods to execute some command, with
 * time taken (and a timeout value)
 */
object Runnable {
  /**
   * Run the command, without worrying about timing.
   * Blocks until execution is done, or times out.
   *
   * @param timeout number of seconds before timeout.
   */
  def executeProc(pb: ProcessBuilder, timeout: Int = 300): ExecutionOutput = {
    // Collected lines from proc's STDOUT, ignore from STDERR.
    val stdoutLines = ArrayBuffer[String]()
    val stderrLines = ArrayBuffer[String]()
    val collectAllLogger = ProcessLogger(line => stdoutLines += line,
                                         line => stderrLines += line)

    // An IOException is thrown if `cmd` doesn't refer to an executable file.

    val proc = pb.run(collectAllLogger)

    // Use Future/Await to handle timeout

    val executeFuture = Future {
      // Block until proc done
      val exitVal = proc.exitValue()

      new ExecutionOutput(stdoutLines.toArray, stderrLines.toArray, exitVal)
    }

    try {
      Await.result(executeFuture, timeout seconds)
    } catch {
      case ex: TimeoutException => {
        proc.destroy()

        new ExecutionOutput(Array(), Array("TIMEOUT"), -2, timedOut = true)
      }
    }
  }

  /**
   * Runs the command given by `command`, returns tuple of `(output, time taken)`.
   */
  def execute(command: String, timeout: Int = 300): (ExecutionOutput, Long) = {
    val startTime = System.currentTimeMillis

    val execOutput = executeProc(command, timeout)

    val endTime = System.currentTimeMillis

    (execOutput, endTime - startTime)
  }
}
