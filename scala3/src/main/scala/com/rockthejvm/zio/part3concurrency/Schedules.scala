package com.rockthejvm.zio.part3concurrency

import zio.*
import com.rockthejvm.zio.utils.{debugThread, succeedZIO}

object Schedules extends ZIOAppDefault {

  val aZIO = Random.nextBoolean.flatMap { flag =>
    if (flag) then succeedZIO("fetched value!")
    else succeedZIO("failure...") *> ZIO.fail("error")
  }

  val aRetriedZIO = aZIO.retry(Schedule.recurs(10))

  // schedules are data structures that describe how effects should be timed
  val oneTimeSchedule = Schedule.once
  val recurrentSchedule = Schedule.recurs(10)
  val fixedIntervalSchedule = Schedule.spaced(1.second) //retries every 1s until a success is returned

  // exponential backoff
  val exBackoffSchedule = Schedule.exponential(1.second, 2.0) // until a success is returned
  val fiboSchedule = Schedule.fibonacci(1.second) // until a success is returned

  // combinators
  val recurrentAndSpaced = Schedule.recurs(5) && Schedule.spaced(2.second) //every attempt
  val recurrentThenSpaced = Schedule.recurs(2) ++ Schedule.spaced(3.second) //first run 2 times, if fail then spaced

  // Schedule have
  // R = environment
  // I = input (errors in the case of .retry, values in the case of .repeat)
  // O = output (values for the next schedule so that you can do something with them)
  val totalElapsed = Schedule.spaced(2.second) >>> Schedule.elapsed.map(time => println(s"total time elapsed: $time"))

  override def run: ZIO[Any, Any, Any] = aZIO.retry(totalElapsed)
}
