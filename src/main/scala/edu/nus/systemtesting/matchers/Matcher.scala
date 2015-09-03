package edu.nus.systemtesting.matchers

/**
 * Models any matching behavior between two texts.
 * The matcher only deals in text because of the requirements of the system
 */
trait Matcher {
  def matches(): Boolean
}