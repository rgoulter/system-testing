package edu.nus.systemtesting

import java.io.FileNotFoundException
import java.util.concurrent.TimeoutException
import scala.concurrent.{ Await, Future }
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.DurationInt
import scala.sys.process.{ stringToProcess, ProcessLogger }
import scala.collection.mutable.ArrayBuffer

import com.typesafe.config.ConfigFactory

/**
 * This trait provides methods to execute some [[formCommand]], with
 * time taken (and a timeout value)
 *
 * Uses the `TIMEOUT` value from the application's `Config`.
 */
trait Runnable {
  /** The full command to be executed by the [[Runnable]] object. */
  def formCommand: String

  /**
   * Run the command, without worrying about timing.
   * Blocks until execution is done, or times out.
   */
  private def executeInner: (Int, String) = {
    val cmd = formCommand
    val timeout = ConfigFactory.load().getInt("TIMEOUT")

    // Collected lines from proc's STDOUT, ignore from STDERR.
    val stdoutLines = ArrayBuffer[String]()
    val collectAllLogger = ProcessLogger(line => stdoutLines += line,
                                         line => ())

    // An IOException is thrown if `cmd` doesn't refer to an executable file.

    println(cmd)
    val proc = cmd.run(collectAllLogger)

    // Use Future/Await to handle timeout

    val executeFuture = Future {
      // Block until proc done
      val exitVal = proc.exitValue()

      (exitVal, stdoutLines.mkString("\n"))
    }

    try {
      Await.result(executeFuture, timeout seconds)
    } catch {
      case ex: TimeoutException => {
        proc.destroy()

        (-2, "The computation timed out")
      }
    }
  }

  /**
   * Runs the command given by [[formCommand]], returns tuple of `(output, time taken)`.
   */
  def execute: (String, Long) = {
    var endTime = 0L
    var startTime = System.currentTimeMillis

    val (exitVal, output) = executeInner

    endTime = System.currentTimeMillis

    (output, endTime - startTime)
  }
}
