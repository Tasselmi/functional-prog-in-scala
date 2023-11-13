package com.rockthejvm.zio.part4coordination

import zio.*
import com.rockthejvm.zio.utils.{debugThread, succeedZIO}
import scala.collection.immutable.Queue

abstract class Mutex {
  def acquire: UIO[Unit]
  def release: UIO[Unit]
}

object Mutex {
  type Signal = Promise[Nothing, Unit]
  case class State(isLocked: Boolean, waiting: Queue[Signal])
  val unlocked = State(false, Queue.empty)

  def make: UIO[Mutex] = Ref.make(unlocked).map(createSimpleMutex)

  def createInterruptibleMutex(stateRef: Ref[State]) = new Mutex {
    override def acquire: UIO[Unit] = ZIO.interruptibleMask { restore =>
      Promise.make[Nothing, Unit] /*UIO[Promise[Nothing, Unit]]*/ flatMap { signal =>

        val cleanup: UIO[Unit] = stateRef.modify {
          case State(flag, waiting) =>
            val remaining = waiting.filterNot(_ eq signal) //除了等待的那个以外的
            // remaining == waiting 说明要清除的信号不在队列里面
            val decision = if (remaining == waiting) ZIO.unit else release

            decision -> State(flag, remaining)
        }.flatten

        stateRef.modify {
          case State(false, _) => ZIO.unit -> State(true, Queue.empty)
          case State(true, waiting) =>
            // signal.await是阻塞点，可能被打断，打断时候要清理现场
            restore(signal.await).onInterrupt(cleanup) -> State(true, waiting.enqueue(signal))
        }.flatten
      }
    }

    override def release: UIO[Unit] = stateRef.modify { // UIO[B] 其中B本身就是个UIO[Unit]，所以需要flatten下
      case State(false, _) => (ZIO.unit, unlocked)
      case State(true, waiting) if (waiting.isEmpty) => (ZIO.unit, unlocked)
      case State(true, wt) => (wt.head.succeed(()).unit, State(true, wt.tail))
    }.flatten
  }

  def createSimpleMutex(stateRef: Ref[State]) = new Mutex {
    /**
      * change the state of the Ref
      *  - if the mutex is unlocked, lock it
      *  - if the mutex is locked, state becomes (true, queue + new signal) and wait on that signal
      */
    override def acquire = Promise.make[Nothing, Unit].flatMap { signal =>
      stateRef.modify { // modify修改了 A => (B, A) 修改了A返回了B，日了狗哎
        case State(false, _) => (ZIO.unit, State(true, Queue.empty))
        case State(true, wt) => (signal.await, State(true, wt.enqueue(signal)))
      }.flatten
    }

    /**
      * change the state of the Ref
      *  - if the mutex is unlocked, leave the state unchanged
      *  - if the mutex is locked
      *   - if the queue is empty, unlock the mutex
      *   - if the queue is non-empty, take a signal out of the queue and complete it
      */
    override def release = stateRef.modify {
      case State(false, _) => (ZIO.unit, unlocked)
      case State(true, waiting) if (waiting.isEmpty) => (ZIO.unit, unlocked)
      case State(true, wt) => (wt.head.succeed(()).unit, State(true, wt.tail))
    }.flatten
  }


}


object MutexPlayground extends ZIOAppDefault {

  def workInCriticalRegion(): UIO[Int] = ZIO.sleep(1.second) *> Random.nextIntBounded(100)

  def demoNonLockingTasks() = ZIO.collectAllParDiscard((1 to 10).map { i =>
    for {
      _ <- succeedZIO(s"[Task $i] working...")
      result <- workInCriticalRegion()
      _ <- succeedZIO(s"[Task $i] got result: $result")
    } yield ()
  })

  def createTask(id: Int, mutex: Mutex): UIO[Int] = {
    val task = for {
      _ <- ZIO.succeed(s"[task $id] waiting for mutex...").debugThread
      _ <- mutex.acquire
      // critical region start
      _ <- ZIO.succeed(s"[task $id] mutex acquired, working...").debugThread
      result <- workInCriticalRegion().onInterrupt(mutex.release)
      _ <- ZIO.succeed(s"[task $id] got result: $result, releasing mutex").debugThread
      // critical region end
      _ <- mutex.release
    } yield result

    task
      .onInterrupt(ZIO.succeed(s"[task $id] was interrupted.").debugThread)
      .onError(cause => ZIO.succeed(s"[task $id] ended in error: $cause"))
  }

  def demoLockingTasks() = for {
    mutex <- Mutex.make
    _ <- ZIO.collectAllParDiscard((1 to 10).toList.map(i => createTask(i, mutex)))
  } yield ()

  def createInterruptingTask(id: Int, mutex: Mutex): UIO[Int] =
    if (id % 2 == 0)
      createTask(id, mutex)
    else for {
      fib <- createTask(id, mutex).fork
      _ <- ZIO.sleep(2500.millis) *> ZIO.succeed(s"interrupting task $id").debugThread *> fib.interrupt
      result <- fib.join
    } yield result


  /*
    _ _ X _ _ _ _ _ _ _
    2.5s => all the odd tasks will be interrupted
   */
  def demoInterruptingTasks() = for {
    mutex <- Mutex.make
    fib1 <- createInterruptingTask(1, mutex).fork
    fib2 <- createInterruptingTask(2, mutex).fork
    fib3 <- createInterruptingTask(3, mutex).fork
    fib4 <- createInterruptingTask(4, mutex).fork
    fib5 <- createInterruptingTask(5, mutex).fork
    fib6 <- createInterruptingTask(6, mutex).fork
    fib7 <- createInterruptingTask(7, mutex).fork
    fib8 <- createInterruptingTask(8, mutex).fork
    fib9 <- createInterruptingTask(9, mutex).fork
    fib10 <- createInterruptingTask(10, mutex).fork
    _ <- fib1.await
    _ <- fib2.await
    _ <- fib3.await
    _ <- fib4.await
    _ <- fib5.await
    _ <- fib6.await
    _ <- fib7.await
    _ <- fib8.await
    _ <- fib9.await
    _ <- fib10.await
  } yield ()

  override def run: ZIO[Any, Any, Any] = demoInterruptingTasks()
}
