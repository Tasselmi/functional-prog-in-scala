package com.rockthejvm.zio.part4coordination

import zio.*
import com.rockthejvm.zio.utils.{debugThread, succeedZIO}

object Semaphores extends ZIOAppDefault {
  
  val sema = Semaphore.make(1)

  def doWorkWhileLogin(): UIO[Int] =
    ZIO.sleep(1.second) *> Random.nextIntBounded(100)

  def login(id: Int, sem: Semaphore): UIO[Int] =
    succeedZIO(s"[Task $id] waiting to log in") *> sem.withPermit {
      for {
        _ <- succeedZIO(s"[Task $id] logged in, working...")
        res <- doWorkWhileLogin()
        _ <- succeedZIO(s"[Task $id] done: $res")
      } yield res
    }

  def demoSemaphore() = for {
    sem <- Semaphore.make(1) // ==1 ==Mutex
    f1 <- login(1, sem).fork
    f2 <- login(2, sem).fork
    f3 <- login(3, sem).fork
    _ <- f1.join
    _ <- f2.join
    _ <- f3.join
  } yield ()

  def loginWeighted(n: Int, sem: Semaphore): UIO[Int] =
    succeedZIO(s"[Task $n] waiting to log in with $n permits") *> sem.withPermits(n) {
      for {
        _ <- succeedZIO(s"[Task $n] logged in, working...")
        res <- doWorkWhileLogin()
        _ <- succeedZIO(s"[Task $n] done: $res")
      } yield res
    }

  def demoSemaphoreWeighted() = for {
    sem <- Semaphore.make(2) // ==1 ==Mutex
    f1 <- loginWeighted(1, sem).fork
    f2 <- loginWeighted(2, sem).fork
    f3 <- loginWeighted(3, sem).fork
    _ <- f1.join
    _ <- f2.join
    _ <- f3.join
  } yield ()


  /**
    * Exercise
    *
    */
  val tasksFixed = sema.flatMap { sem =>
    ZIO.collectAllPar((1 to 10).map { id =>
      for {
        _ <- succeedZIO(s"[Task $id] waiting to log in")
        resOuter <- sem.withPermit {
          for {
            _ <- succeedZIO(s"[Task $id] logged in, working...")
            res <- doWorkWhileLogin()
            _ <- succeedZIO(s"[Task $id] done: $res")
          } yield res
        }
      } yield resOuter
    })
  }

  override def run: ZIO[Any, Any, Any] = tasksFixed
}
