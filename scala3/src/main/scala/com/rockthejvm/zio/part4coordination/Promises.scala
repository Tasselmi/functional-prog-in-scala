package com.rockthejvm.zio.part4coordination

import zio.*
import com.rockthejvm.zio.utils.{debugThread, succeedZIO}

object Promises extends ZIOAppDefault {

  val aPromise: UIO[Promise[Throwable, Int]] = Promise.make[Throwable, Int]

  val reader: ZIO[Any, Throwable, Int] = aPromise.flatMap(p => p.await)
  val writer: ZIO[Any, Nothing, Boolean] = aPromise.flatMap(p => p.succeed(42))

  def demoPromise() = {
    def consumer(promise: Promise[Throwable, Int]): Task[Unit] = for {
      _ <- succeedZIO("[consumer] waiting for result...")
      mol <- promise.await
      _ <- succeedZIO(s"[consumer] I got the result: $mol")
    } yield ()

    def producer(promise: Promise[Throwable, Int]): Task[Unit] = for {
      _ <- succeedZIO("[producer] crunching numbers...")
      _ <- ZIO.sleep(3.seconds)
      _ <- succeedZIO("[producer] complete.")
      mol <- ZIO.succeed(42)
      _ <- promise.succeed(mol)
    } yield ()

    for {
      promise <- Promise.make[Throwable, Int]
      _ <- consumer(promise) zipPar producer(promise)
    } yield ()
  }

  /*
  - purely functional block on a fiber until you get a signal from another fiber
  - waiting on a value which may not yet be available, without thread starvation
  - inter-fiber communication
 */

  // simulate downloading from multiple parts
  val fileParts = List("I ", "love S", "cala", " with pure FP an", "d ZIO! <EOF>")
  def downloadFileWithRef(): Task[Unit] = {     // busy waiting
    def downloadFile(contentRef: Ref[String]): Task[Unit] =
      ZIO.collectAllDiscard(
        fileParts.map { part =>
          ZIO.succeed(s"got '$part'").debugThread *> ZIO.sleep(1.second) *> contentRef.update(_ + part)
        }
      )

    def notifyFileComplete(contentRef: Ref[String]): Task[Unit] = for {
      file <- contentRef.get
      _ <-  if (file.endsWith("<EOF>")) ZIO.succeed("File download complete.").debugThread
      else ZIO.succeed("downloading...").debugThread *> ZIO.sleep(500.millis) *> notifyFileComplete(contentRef)
    } yield ()

    for {
      contentRef <- Ref.make("")
      _ <- downloadFile(contentRef) zipPar notifyFileComplete(contentRef)
    } yield ()
  }

  def downloadFileWithRefPromise(): Task[Unit] = {
    def downloadFile(contentRef: Ref[String], promise: Promise[Throwable, String]): Task[Unit] =
      ZIO.collectAllDiscard(
        fileParts.map { part =>
          for {
            _ <- succeedZIO(s"got '$part")
            _ <- ZIO.sleep(1.second)
            file <- contentRef.updateAndGet(_ + part)
            _ <- if (file.endsWith("<EOF>")) then promise.succeed(file) else ZIO.unit
          } yield ()
        }
      )

    def notifyFileComplete(promise: Promise[Throwable, String]): Task[Unit] = for {
      _ <- succeedZIO("downloading...")
      file <- promise.await
      _ <- succeedZIO(s"file download complete: $file")
    } yield ()

    for {
      ref <- Ref.make("")
      pm <- Promise.make[Throwable, String]
      _ <- downloadFile(ref, pm) zipPar notifyFileComplete(pm)
    } yield ()
  }


  /**
    * Exercises
    * 1. Write a simulated "egg boiler" with two ZIOs
    *  - one increments a counter every 1s
    *  - one waits for the counter to become 10, after which it will "ring a bell"
    *
    * 2. Write a "race pair"
    *  - use a Promise which can hold an Either[exit for A, exit for B]
    *  - start a fiber for each ZIO
    *  - on completion (with any status), each ZIO needs to complete that Promise
    *    (hint: use a finalizer)
    *  - waiting on the Promise's value can be interrupted!
    *  - if the whole race is interrupted, interrupt the running fibers
    */
  def eggBoiler() = {
    def eggReady(signal: Promise[Nothing, Unit]): UIO[Unit] = for {
      _ <- succeedZIO("egg boiling on some other fiber, waiting...")
      _ <- signal.await
      _ <- succeedZIO("EGG READY!")
    } yield ()

    def tickingClock(ticks: Ref[Int], signal: Promise[Nothing, Unit]): UIO[Unit] = for {
      _ <- ZIO.sleep(1.second)
      count <- ticks.updateAndGet(_ + 1)
      _ <- succeedZIO(count)
      _ <- if (count >= 10) then signal.succeed(()) else tickingClock(ticks, signal)
    } yield ()

    for {
      ticks <- Ref.make(0)
      signal <- Promise.make[Nothing, Unit]
      _ <- eggReady(signal) zipPar tickingClock(ticks, signal)
    } yield ()
  }

  def racePair[R, E, A, B](zio1: => ZIO[R, E, A], zio2: ZIO[R, E, B]):
    ZIO[R, Nothing, Either[(Exit[E, A], Fiber[E, B]), (Fiber[E, A], Exit[E, B])]] = ZIO.uninterruptibleMask { restore =>

    for {
      promise <- Promise.make[Nothing, Either[Exit[E, A], Exit[E, B]]]
      fibA <- restore(zio1).onExit(a => promise.succeed(Left(a))).fork
      fibB <- restore(zio2).onExit(b => promise.succeed(Right(b))).fork

      result <- restore(promise.await).onInterrupt {
        for {
          interruptA <- fibA.interrupt.fork
          interruptB <- fibB.interrupt.fork
          _ <- interruptA.join
          _ <- interruptB.join
        } yield ()
      }
    } yield result match {
      case Left(exitA) => Left((exitA, fibB))
      case Right(exitB) => Right((fibA, exitB))
    }
  }

  def demoRacePair = {
    val zio1 = ZIO.sleep(4.second).as(1).onInterrupt(succeedZIO("first interrupted"))
    val zio2 = ZIO.sleep(2.second).as(2).onInterrupt(succeedZIO("second interrupted"))

    racePair(zio1, zio2).flatMap {
      case Left((exit1, fib2)) => fib2.interrupt *> succeedZIO("first won") *> succeedZIO(exit1)
      case Right((fib1, exit2)) => fib1.interrupt *> succeedZIO("second won") *> succeedZIO(exit2)
    }
  }

  override def run: ZIO[Any, Any, Any] = demoRacePair
}
