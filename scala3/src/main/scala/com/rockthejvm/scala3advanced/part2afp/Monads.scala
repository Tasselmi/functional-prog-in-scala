package com.rockthejvm.`scala3advanced`.part2afp

object Monads {

  ////////////////////////////////////////////////////////////////////////////////
  //  1. monad is wrapper of data, like List, Option, Future
  ////////////////////////////////////////////////////////////////////////////////
  def listStory(): Unit = {
    val aList = List(1, 2, 3)

    val listMultiply = for {
      x <- List(1, 2, 3)
      y <- List(2, 3, 4)
    } yield x * y

    //for comprehension = flatmap + map
    val listMultiplyV2 = List(1, 2, 3).flatMap(x => List(2, 3, 4).map(y => x * y))

    println(listMultiply)
    println(listMultiplyV2)

    //// prop 1: left identity
    val f = (x: Int) => List(x, x + 1)
    val g = (x: Int) => List(x * 10, x * 100)
    val pure = (x: Int) => List(x)

    val leftIdentity = pure(42).flatMap(f) == f(42)

    //// prop 2: right identity
    val rightIdentity = aList.flatMap(pure) == aList

    //// prop 3: associativity  对每个元素先执行f变换，再执行g变换
    val associativity = aList.flatMap(f).flatMap(g) == aList.flatMap(x => f(x).flatMap(g))

    println(leftIdentity)
    println(rightIdentity)
    println(associativity)

    //monads == chain dependent computations
  }

  listStory()


  ////////////////////////////////////////////////////////////////////////////////
  //  2. exercise: is this a monad ? YES. PossibleMonad == IO
  ////////////////////////////////////////////////////////////////////////////////
  case class PossibleMonad[A](unsafeRun: () => A) {
    def map[B](f: A => B): PossibleMonad[B] =
      PossibleMonad(() => f(unsafeRun()))

    def flatMap[B](f: A => PossibleMonad[B]): PossibleMonad[B] =
      //PossibleMonad(f(unsafeRun()).unsafeRun)  //如果这样写，那么不是lazy的，也就不是计算描述了，因为会先调用 f(unsafeRun()) 这一部分
      PossibleMonad(() => f(unsafeRun()).unsafeRun())
  }

  object PossibleMonad {
    @scala.annotation.targetName("pure")
    def apply[A](value: => A): PossibleMonad[A] =
      PossibleMonad(() => value)
  }

  def possibleMonadStory(): Unit = {
    val f = (x: Int) => PossibleMonad(x + 1)
    val g = (x: Int) => PossibleMonad(x * 2)
    val pure = (x: Int) => PossibleMonad(x)

    val aPossibleMonad = PossibleMonad(42)

    //prop 1: left identity
    val leftIdentity = pure(42).flatMap(f) == f(42) //这个主要是测试pure本身的
    println(leftIdentity)

    //prop 2: right identity
    val rightIdentity = aPossibleMonad.flatMap(pure) == aPossibleMonad
    println(rightIdentity)

    //prop 3: associativity
    val asso = aPossibleMonad.flatMap(f).flatMap(g) == aPossibleMonad.flatMap(x => f(x).flatMap(g))
    println(asso)

    //false negative
    //real tests: values produced + side effect ordering
    println(PossibleMonad(10) == PossibleMonad(10))
    val leftIdentityV2 = pure(42).flatMap(f).unsafeRun() == f(42).unsafeRun()
    println(leftIdentityV2)
    val rightIdentityV2 = aPossibleMonad.flatMap(pure).unsafeRun() == aPossibleMonad.unsafeRun()
    println(rightIdentityV2)
    val assoV2 = aPossibleMonad.flatMap(f).flatMap(g).unsafeRun() == aPossibleMonad.flatMap(x => f(x).flatMap(g)).unsafeRun()
    println(assoV2)

  }

  possibleMonadStory()


  ////////////////////////////////////////////////////////////////////////////////
  //  3. monad that describe/encapsulate computation: any computation that might perform side effect
  ////////////////////////////////////////////////////////////////////////////////
  def possibleMonadExample(): Unit = {
    val oneMonad: PossibleMonad[Int] = PossibleMonad {
      println("printing my first possible monad")
      //do some computations
      42
    }

    val anotherMonad: PossibleMonad[String] = PossibleMonad {
      println("my second possible monad")
      "scala"
    }

    val aForComprehension: PossibleMonad[String] = for {  //computations are described, not executed
      num <- oneMonad
      lang <- anotherMonad
    } yield s"$num-$lang"

    val anotherForComp: PossibleMonad[String] =
      oneMonad.flatMap(num => anotherMonad.map(lang => s"$num-$lang"))
  }


  def main(args: Array[String]): Unit = {
    possibleMonadExample()
    Thread.sleep(1000L)
  }
}
