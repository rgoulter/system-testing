package edu.nus.systemtesting.hipsleek.app

/**
 * @author richardg
 */
class UsesRepository(config: AppConfig) {
  val repoDir = config.repoDirOrDie

  val repo = config.repoOrDie
}