package edu.nus.systemtesting

import java.io.FileNotFoundException
import java.util.concurrent.TimeoutException

import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.concurrent.duration.DurationInt
import scala.sys.process.stringToProcess

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
  private def executeInner: String = {
    val cmd = formCommand
    val timeout: Int = ConfigFactory.load().getInt("TIMEOUT")

    // Use Future/Await to handle timeout

    val executeFuture: Future[String] = Future {
      println(cmd)
      val result: String = cmd.!!
      result
    }

    try {
      Await.result(executeFuture, timeout seconds)
    } catch {
      case ex: TimeoutException =>
        return "The above computation timed out"
      case ex: FileNotFoundException =>
        "The file could not be found, please check the executable paths"
      case ex: RuntimeException =>
        "Non-zero exit code"
    }
  }

  /**
   * Run the command given by [[formCommand]], returning the output and the time taken.
   */
  def execute: (String, Long) = {
    var endTime: Long = 0
    var startTime = System.currentTimeMillis

    val result = executeInner

    endTime = System.currentTimeMillis

    (result, endTime - startTime)
  }
}
