package edu.nus.systemtesting

/**
 * @author richardg
 */
trait SystemPreparation {
  /**
   * Run at the beginning of a [[TestSuite]] `runAllTests`.
   * It's intended for things like "running make", as opposed to
   * a more comprehensive provisioning.
   *
   * Return value should be whether provisioning worked,
   * and any remarks which need to be made. (e.g. warning about
   * absence of some program).
   */
  def prepare() : (Boolean, Iterable[String])
}