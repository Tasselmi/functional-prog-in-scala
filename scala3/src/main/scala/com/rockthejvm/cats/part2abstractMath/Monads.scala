package com.rockthejvm.cats.part2abstractMath

import scala.concurrent.ExecutionContext
import java.util.concurrent.Executors
import scala.concurrent.Future

object Monads {

  /**
    * Pattern
    *   - wrapping a value into a monadic value
    *   - the flatMap mechanism
    * 
    * MONADS
    * 
    */
  trait Monad[M[_]] {
    def pure[A](value: A): M[A]
    def flatMap[A, B](ma: M[A])(f: A => M[B]): M[B]
  }

  // cats monad
  import cats.Monad
  import cats.instances.option.*
  val optionMonad = Monad[Option]
  val anOption    = optionMonad.pure(4)

  val aTransformedOption = optionMonad.flatMap(anOption) { x =>
    if (x % 3 == 0) Some(x + 1)
    else None
  }

  import cats.instances.future.*
  import ExecutionContext.Implicits.global
  // implicit  val ec: ExecutionContext = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(4))
  val futureMonad        = Monad[Future]
  val aFuture            = futureMonad.pure(43)
  val aTransformedFuture = futureMonad.flatMap(aFuture)(x => Future(x + 44))

  def main(args: Array[String]): Unit = {
    println("hello world")
    println(aTransformedOption)
    
    Thread.sleep(200)
    println(aTransformedFuture)
  }
}
