package com.rockthejvm.cats.part2abstractMath

object MonadsPractice {

  import cats.Monad
  import cats.instances.list.*
  import cats.instances.either.*

  val aManualEither: Either[String, Int] = Right(42)
  type LoadingOr[T] = Either[String, T]
  type ErrorOr[T]   = Either[Throwable, T]
  val loadingMonad = Monad[LoadingOr]
  val anEither     = loadingMonad.pure(45)

  val aChangedLoading = loadingMonad.flatMap(anEither)(a =>
    if (a % 2 == 0) Right(a + 1) else Left("loading...")
  )

  def main(args: Array[String]): Unit = {}
}
