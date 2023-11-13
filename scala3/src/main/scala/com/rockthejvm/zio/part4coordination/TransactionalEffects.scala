package com.rockthejvm.zio.part4coordination

import zio.*
import zio.stm.*
import com.rockthejvm.zio.utils.{debugThread, succeedZIO}

/**
  * Software Transactional Memory
  */
object TransactionalEffects extends ZIOAppDefault {

  val anSTM: USTM[Int] = STM.succeed(42)
  val failedSTM = STM.fail("something bad")
  val attemptSTM = STM.attempt(42 / 0)

  // STM vs ZIO
  // compose STMs to obtain other STMs
  // evaluation is fully atomic
  // "commit" -- 提交
  val anAtomicEffect: ZIO[Any, Throwable, Int] = attemptSTM.commit

  // example
  def transferMoney(sender: Ref[Long], receiver: Ref[Long], amount: Long) = for {
    senderBalance <- sender.get
    _ <- if (senderBalance < amount) ZIO.fail("tranfer failed: insufficient funds") else ZIO.unit
    _ <- sender.update(_ - amount)
    _ <- receiver.update(_ + amount)
    newBalance <- sender.get
  } yield newBalance

  def exploitBuggyBank() = for {
    sender <- Ref.make(1000L)
    receiver <- Ref.make(0L)
    fib1 <- transferMoney(sender, receiver, 1000L).fork
    fib2 <- transferMoney(sender, receiver, 1000L).fork
    _ <- (fib1 zip fib2).join
    _ <- receiver.get.debugThread  // should never be > 1000
  } yield ()

  def loop(i: Int, effect: ZIO[Any, String, Unit]): ZIO[Any, Nothing, Unit] =
    if (i > 10000) then ZIO.unit
    else effect.ignore *> loop(i + 1, effect)

  // STM implementation
  def transferMoneyTransational(sender: TRef[Long], receiver: TRef[Long], amount: Long): ZSTM[Any, String, Long] = for {
    senderBalance <- sender.get
    _ <- if (senderBalance < amount) STM.fail("tranfer failed: insufficient funds") else STM.unit
    _ <- sender.update(_ - amount)
    _ <- receiver.update(_ + amount)
    newBalance <- sender.get
  } yield newBalance

  def cannotExploit() = for {
    sender <- TRef.make(1000L).commit // TRef保证同一时间只有一个能update
    receiver <- TRef.make(0L).commit
    fib1 <- transferMoneyTransational(sender, receiver, 1000L).commit.fork
    fib2 <- transferMoneyTransational(sender, receiver, 1000L).commit.fork
    _ <- (fib1 zip fib2).join
    _ <- receiver.get.commit.debugThread  // should never be > 1000
  } yield ()


  /**
    * STM data structures
    *
    *  - atomic variable: TRef
    *  - same API: get, update, modify, set
    */
  val aVariable: USTM[TRef[Int]] = TRef.make(42)

  val specifiedValuesTArry: USTM[TArray[Int]] = TArray.make(1, 2, 3)
  val iterableArray: USTM[TArray[Int]] = TArray.fromIterable(List(1, 2, 3, 4, 5))
  val tArrayElement: ZSTM[Any, Nothing, Int] = for {
    ta <- iterableArray
    elem <- ta.apply(2)
  }  yield elem
  val tArrayUpdateElem = for {
    ta <- iterableArray
    _ <- ta.update(1, x => x + 10)
  } yield ta

  // TSet
  // TMap
  // TQueue
  // TPriorityQueue

  /**
    * concurrent coordination
    *
    */
  val tPromise: USTM[TPromise[String, Int]] = TPromise.make[String, Int]
  val tPromiseAwait: ZSTM[Any, Nothing, Unit] = for {
    p <- tPromise
    _ <- p.succeed(100)
  } yield ()

  val tSemaphoreEffect = TSemaphore.make(10L)
  val semaphoreAcq = for {
    sema <- tSemaphoreEffect
    _ <- sema.acquire
    _ <- sema.release
  } yield ()
  val semWithPermit: UIO[Int] = tSemaphoreEffect.commit.flatMap(s => ZIO.succeed(42).debugThread)


  // TReentrantLock - can acquire the same lock multiple times without deadlock
  // readers-writers problem
  // has two locks: read lock(lower priority) and write lock(higher priority)
  val reentrantLockEffect: USTM[TReentrantLock] = TReentrantLock.make
  val demoReentrantLock = for {
    lock <- reentrantLockEffect
    _ <- lock.acquireRead   //acquires the read lock
    _ <- STM.succeed(100)   //critical section, only those that acquire read lock can access
    rl <- lock.readLocked   //status of the lock, whether is read-locked, true in this case
    wl <- lock.writeLocked  //same for wirter
  } yield ()

  def demoReadersWriters(): ZIO[Any, Nothing, Unit] = {
    def read(i: Int, lock: TReentrantLock): ZIO[Any, Nothing, Unit] = for {
      _ <- lock.acquireRead.commit
      // critical region
      _ <- succeedZIO(s"[task ${i}] taken the read lock, reading...")
      time <- Random.nextIntBounded(1000)
      _ <- ZIO.sleep(time.millis)
      res <- Random.nextIntBounded(100)   //actual computation
      _ <- succeedZIO(s"[task ${i}] read value $res")
      // critical region end
      _ <- lock.releaseRead.commit
    } yield ()

    def write(lock: TReentrantLock): ZIO[Any, Nothing, Unit] = for {
      _ <- ZIO.sleep(200.millis)
      _ <- succeedZIO("[writer] trying to write...")
      _ <- lock.acquireWrite.commit
      // critical region
      _ <- succeedZIO("[writer]  I'm able to write!")
      // critical region end
      _ <- lock.releaseWrite.commit
    } yield ()

    for {
      lock <- TReentrantLock.make.commit
      readersFib <- ZIO.collectAllParDiscard((1 to 10).map(i => read(i, lock))).fork
      writerFib <- write(lock).fork
      _ <- (readersFib zip writerFib).join
    } yield ()
  }

  override def run: ZIO[Any, Any, Any] = demoReadersWriters()
}
