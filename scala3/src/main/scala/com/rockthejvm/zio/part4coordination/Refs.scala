package com.rockthejvm.zio.part4coordination

import zio.*
import com.rockthejvm.zio.utils.{debugThread, succeedZIO}

import java.util.concurrent.TimeUnit

object Refs extends ZIOAppDefault {

  // purely functional atomic references
  val atomicMol: UIO[Ref[Int]] = Ref.make(42)

  val mol: ZIO[Any, Nothing, Int] = atomicMol.flatMap { ref =>
    ref.get
  }

  // example: distributing work
  def demoConcurrentWokrImpure = {
    var count = 0

    def task(workload: String) = {
      val wordCount = workload.split(" ").length
      for {
        _ <- succeedZIO(s"counting words for [$workload]: $wordCount")
        newCount <- ZIO.succeed(count + wordCount)
        _ <- succeedZIO(s"new total: $newCount")
        _ <- ZIO.succeed(count += wordCount)
      } yield ()
    }

    val effects = List("I love ZIO", "This Ref thing is cool", "Daniel writes a LOT of code!").map(task)
    ZIO.collectAllParDiscard(effects)
  }

  def demoConcurrentWorkPure() = {
    def task(workload: String, total: Ref[Long]) = {
      val wordCount = workload.split(" ").length
      for {
        _ <- succeedZIO(s"counting words for [$workload]: $wordCount")
        newTotal <- total.updateAndGet(old => old + wordCount)
        _ <- succeedZIO(s"new total: $newTotal")
      } yield ()
    }

    for {
      counter <- Ref.make(0L)
      effects = List("I love ZIO", "This Ref thing is cool", "Daniel writes a LOT of code!").map(s => task(s, counter))
      _ <- ZIO.collectAllParDiscard(effects)
    } yield ()
  }

  /**
    * Exercises
    *
    */
  def tickingClockPure() = {
    def tickingClock(ticks: Ref[Long]): UIO[Unit] = for {
      _ <- ZIO.sleep(1.second)
      _ <- Clock.currentTime(TimeUnit.MILLISECONDS).debugThread
      _ <- ticks.update(old => old + 1)
      _ <- tickingClock(ticks)
    } yield ()

    def printTicks(ticks: Ref[Long]): UIO[Unit] = for {
      _ <- ZIO.sleep(5.seconds)
      t <- ticks.get
      _ <- succeedZIO(s"TICKS: ${t}")
      _ <- printTicks(ticks)
    } yield ()

    for {
      counter <- Ref.make(0L)
      _ <- tickingClock(counter) zipPar printTicks(counter)
    } yield ()
  }

  def tickingClockPureBuggy(): ZIO[Any, Nothing, Unit] = {
    val ticksRef = Ref.make(0L)

    def tickingClock: UIO[Unit] = for {
      ticks <- ticksRef
      _ <- ZIO.sleep(1.second)
      _ <- Clock.currentTime(TimeUnit.MILLISECONDS).debugThread
      _ <- ticks.update(old => old + 1)
      _ <- tickingClock
    } yield ()

    def printTicks: UIO[Unit] = for {
      ticks <- ticksRef
      _ <- ZIO.sleep(5.seconds)
      t <- ticks.get
      _ <- succeedZIO(s"TICKS: ${t}")
      _ <- printTicks
    } yield ()

    (tickingClock zipPar printTicks).unit
  }

  // update function may be run more than once
  // every time only 1 can be successful, the rest will retry again and again
  def demoMultipleUpdates = {
    def task(i: Int, ref: Ref[Int]) =
      ref.modify(old => (println(s"Task $i updating ref at $old to be $i"), i))

    for {
      ref <- Ref.make(0)
      _ <- ZIO.collectAllParDiscard((1 to 10).map(i => task(i, ref)))
    } yield ()
  }

  override def run: ZIO[Any, Any, Any] = demoMultipleUpdates
}
