package com.rockthejvm.zio.part3concurrency

import zio.*
import com.rockthejvm.zio.utils.*

object MasteringInterruptions extends ZIOAppDefault {

  /**
    * interruptions:
    *  - fib.inerrupt
    *  - ZIO.race, ZIO.zipPar, ZIO.collectAllPar
    *  - outliving parent fiber
    */

  //manual interruption
  val aManuallyInterruptedZIO = succeedZIO("compting...") *> ZIO.interrupt *> succeedZIO(42)

  //finalizer
  val effectWithInterruptionFinalizer = aManuallyInterruptedZIO.onInterrupt(succeedZIO("I was interrupted!"))


  /**
    * uninterruptable
    */
  //payment flow to not be interrupted
  val fussyPaymentSystem = (
    succeedZIO("payment running, don't cancel me...") *>
      ZIO.sleep(1.second) *>  //actual payment process
      succeedZIO("payment completed...")
  ).onInterrupt(succeedZIO("MEGA CANCEL OF DOOM"))

  val cancellationOfDoom = for {
    fib <- fussyPaymentSystem.fork
    _ <- ZIO.sleep(500.millis) *> fib.interrupt
    _ <- fib.join
  } yield ()

  //the same
  val atomicPayment = ZIO.uninterruptible(fussyPaymentSystem)
  val atomicPaymentV2 = fussyPaymentSystem.uninterruptible

  val noCancellationOfDoom = for {
    fib <- atomicPayment.fork
    _ <- ZIO.sleep(500.millis) *> fib.interrupt
    _ <- fib.join
  } yield ()


  /**
    * interruptable is regional
    */
  val zio1 = succeedZIO(1)
  val zio2 = succeedZIO(2)
  val zio3 = succeedZIO(3)
  val zioComposed = (zio1 *> zio2 *> zio3).uninterruptible  //all are uninterruptable
  val zioComposedV2 = (zio1 *> zio2.interruptible *> zio3).uninterruptible  //inner scopes override outer scopes


  /**
    * uninterruptableMask
    *
    * example: an autentication service
    *  - input password, can be interrupted, because otherwise it might block the fiber indefinitely
    *  - verify password, which cannot be interrupted once it is triggered
    */
  val inputPassword: ZIO[Any, Nothing, String] = for {
    _ <- succeedZIO("input password: ")
    _ <- succeedZIO("(typing password)")
    _ <- ZIO.sleep(10.seconds)
    pass <- succeedZIO("RockTheJVM!")
  } yield pass

  def verifyPassword(pw: String): ZIO[Any, Nothing, Boolean] = for {
    _ <- succeedZIO("verifying...")
    _ <- ZIO.sleep(2.seconds)
    result <- succeedZIO(pw == "RockTheJVM!")
  } yield result

  val authFlow = ZIO.uninterruptibleMask { irs =>
    // all the flows under the mask are uninterruptible, except the restore part
    for {
      pw <- irs(inputPassword) /* <- restorer makes it interruptible */ .onInterrupt(succeedZIO("Authentication timed out. Try again later."))
      verification <- verifyPassword(pw)
      _ <- if (verification) succeedZIO("Authentication successful.") else succeedZIO("Authentication failed.")
    } yield ()
  }

  val authProgram = for {
    authFib <- authFlow.fork
    _ <- ZIO.sleep(3.seconds) *> succeedZIO("attempting to cancel authentication...") *> authFib.interrupt
    _ <- authFib.join
  } yield ()


  /**
    * Exercises
    *
    */
  // 1
  val cancelBeforeMol = ZIO.interrupt *> succeedZIO(42)
  val unCancelBeforeMol = ZIO.uninterruptible(cancelBeforeMol)

  // 2
  val authProgramV2 = for {
    authFib <- ZIO.uninterruptibleMask(_ => authFlow).fork
    _ <- ZIO.sleep(1.second) *> succeedZIO("attempting to cancel authentication...") *> authFib.interrupt
    _ <- authFib.join
  } yield ()

  // 3
  val threeStepProgram = {
    val sequence = ZIO.uninterruptibleMask { restore =>
      for {
        _ <- restore(succeedZIO("interruptible 1") *> ZIO.sleep(1.second))
        _ <- succeedZIO("uninterruptible 2") *> ZIO.sleep(1.second)
        _ <- restore(succeedZIO("interruptible 3") *> ZIO.sleep(1.second))
      } yield ()
    }

    for {
      fib <- sequence.fork
      _ <- ZIO.sleep(500.millis) *> succeedZIO("INTERRUPTING!") *> fib.interrupt
      _ <- fib.join
    } yield ()
  }

  override def run: ZIO[Any, Any, Any] = threeStepProgram
}
