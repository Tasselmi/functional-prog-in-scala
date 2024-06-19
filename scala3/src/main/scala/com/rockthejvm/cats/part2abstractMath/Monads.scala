package com.rockthejvm.cats.part2abstractMath

import scala.concurrent.ExecutionContext
import java.util.concurrent.Executors
import scala.concurrent.Future
import cats.data.Op

object Monads {

  /**
    * Pattern
    *   - wrapping a value into a monadic value
    *   - the flatMap mechanism
    * 
    * MONADS : higher-kinded type class that provides
    *   - a pure method to wrap a normal value into a monadic value
    *   - a flatMap method to transform monadic values in sequence
    * 
    */
  trait MyMonad[M[_]] {
    def pure[A](value: A): M[A] // A => M[A]
    def flatMap[A, B](ma: M[A])(f: A => M[B]): M[B]
    def map[A, B](ma: M[A])(f: A => B): M[B] = flatMap(ma)(a => pure(f(a)))
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

  // generalize
  // import cats.syntax.monad.*
  val numbersList = List(1, 2, 3, 4)
  val charsList   = List('a', 'b', 'c')

  def getPairs[M[_], A, B](ma: M[A], mb: M[B])(using monad: Monad[M]): M[(A, B)] =
    monad.flatMap(ma)(a => monad.map(mb)(b => (a, b)))

  /**
    * extension methods - weirder imports - pure, flatMap
    */
  import cats.syntax.applicative.* // pure is here
  val oneOption = 1.pure[Option] // implicit Monad[Option] will be use => Some(1)
  val oneList   = 1.pure[List]

  /**
    * for-conprehension for Monad
    */
  import cats.syntax.flatMap.* // flatMap is here
  val oneOptionTranformed = oneOption.flatMap(x => (x + 1).pure[Option])
  // Monad extends Functor, so we can use map method
  val oneOptionMapped   = Monad[Option].map(Option(2))(_ + 1)
  val oneOptionMappedV2 = oneOption.map(_ + 1)

  // map + flatMap => so we can use for-comprehension
  val composeOptionFor = for {
    one <- 1.pure[Option]
    two <- 2.pure[Option]
  } yield one + two

  // shorter implementation of getPairs
  import cats.syntax.functor.* // map is here

  def getPairsV2[M[_]: Monad, A, B](ma: M[A], mb: M[B]): M[(A, B)] =
    // ma flatMap (a => mb map (b => (a, b)))
    for {
      a <- ma
      b <- mb
    } yield (a, b)

  def main(args: Array[String]): Unit = {
    println("hello world")
    println(aTransformedOption)

    Thread.sleep(200)
    println(aTransformedFuture)

    println(getPairsV2(numbersList, charsList))
  }

}
