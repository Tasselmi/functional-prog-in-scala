package com

import org.slf4j.{Logger, LoggerFactory}

package object rockthejvm {

  lazy val logger: Logger = LoggerFactory.getLogger(getClass)
}
