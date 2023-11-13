package com.rockthejvm.zio.part3concurrency

import zio.*
import com.rockthejvm.zio.utils.*
import java.util.concurrent.atomic.AtomicBoolean

object BlockingEffects extends ZIOAppDefault {

  def blockingTask(n: Int): UIO[Unit] = succeedZIO(s"running blocking task ${n}") *>
    succeedZIO(Thread.sleep(5000)) *> blockingTask(n)

  //thread starvation
  val program = ZIO.foreachPar((1 to 100).toList)(blockingTask)

  //blocking thread pool
  val aBlockingZIO = ZIO.attemptBlocking {
    println(s"[${Thread.currentThread().getName}] running a long computation...")
    Thread.sleep(10000)
    42
  }

  //blocking code cannnot (usually) be interrupted
  val tryInterrupting = for {
    blockingFib <- aBlockingZIO.fork
    _ <- ZIO.sleep(1.second) *> succeedZIO("interrupting...") *> blockingFib.interrupt
    mol <- blockingFib.join
  } yield mol

  //can use attemptBlockingInterrupt
  //based on Thread.interrupt -> InterruptedException
  val aBlockingInterruptableZIO = ZIO.attemptBlockingInterrupt {
    println(s"[${Thread.currentThread().getName}] running a long computation...")
    Thread.sleep(10000)
    42
  }

  val tryInterruptingV2 = for {
    blockingFib <- aBlockingInterruptableZIO.fork
    _ <- ZIO.sleep(1.second) *> succeedZIO("interrupting...") *> blockingFib.interrupt
    mol <- blockingFib.join
  } yield mol

  //set a flag/switch
  def interuptableBlockingEffect(runningFlag: AtomicBoolean) = ZIO.attemptBlockingCancelable {
    (1 to 100000).foreach { elem =>
      if (runningFlag.get()) then
        println(elem)
        Thread.sleep(200)
    }
  } (succeedZIO(runningFlag.set(false)))

  val interruptableBlockingDemo = for {
    fib <- interuptableBlockingEffect(new AtomicBoolean(true)).fork
    _ <- ZIO.sleep(2.seconds) *> succeedZIO("interrupting...") *> fib.interrupt
    _ <- fib.join
  } yield ()

  //semantic blocking -- no blocking of threads, de-scheduling the effect/fiber
  val sleepingThread = succeedZIO(Thread.sleep(1000)) //blocking, uninterruptable
  val sleeping = ZIO.sleep(1.second)  //semantically blocking, interruptable

  //yield
  val chainedZIO = (1 to 1000).map(i => succeedZIO(i)).reduce((a, b) => a.debugThread *> b.debugThread)
  val yieldingDemo = (1 to 1000).map(i => succeedZIO(i)).reduce((x, y) => x.debugThread *> ZIO.yieldNow *> y.debugThread)

  override def run: ZIO[Any, Any, Any] = yieldingDemo
}
