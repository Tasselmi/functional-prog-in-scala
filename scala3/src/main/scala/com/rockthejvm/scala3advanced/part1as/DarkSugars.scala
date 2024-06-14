package com.rockthejvm.`scala3advanced`.part1as

import scala.util.{Try, Success, Failure}

object DarkSugars {

  ////////////////////////////////////////////////////////////////////////////////
  //  1. sugar for methods with one argument
  ////////////////////////////////////////////////////////////////////////////////
  def singleArgMethod(arg: Int): Int = arg + 1

  val aMethodCall = singleArgMethod({
    /* long code here */
    42
  })

  val aMethodCallV2 = singleArgMethod {
    42
  }

  /* example: Try, Future */
  val aTryInstance = Try(1)
  val aTryInstanceV2 = Try {
    throw new RuntimeException
  }

  /* example: high-order functions */
  val aIncrementedList = List(1, 2, 3).map { x =>
    //code block
    x + 1
  }


  ////////////////////////////////////////////////////////////////////////////////
  //  2. single abstract method pattern (since scala 2.12)
  ////////////////////////////////////////////////////////////////////////////////
  trait Action {
    def act(x: Int): Int
  }

  val action = new Action {
    override def act(x: Int): Int = x + 1
  }

  val anotherAction: Action = (x: Int) => x + 1
  //anotherAction(1)  not work, it is not a function
  anotherAction.act(1)

  /* example: Runnable */
  val aThread = new Thread(new Runnable {
    override def run(): Unit = println("Hi, Scala")
  })

  val aSweetThread = new Thread(() => println("Hi, Scala"))
  aThread.run()
  aSweetThread.run()


  ////////////////////////////////////////////////////////////////////////////////
  //  3. methods ending in a : are RIGHT-ASSOCIATIVE
  ////////////////////////////////////////////////////////////////////////////////
  val aList = List(1, 2, 3)
  val prependList = 0 :: aList
  aList.::(0)
  val bigList = 0 :: 1 :: 2 :: List(3, 4)
  List(3, 4).::(2).::(1).::(0)

  class MyStream[T] {
    infix def -->:(v: T): MyStream[T] = this
  }

  val myStream = (new MyStream[Int]).-->:(3).-->:(2).-->:(1)
  val myStreamV2 = 1 -->: 2 -->: 3 -->: (new MyStream[Int])


  ////////////////////////////////////////////////////////////////////////////////
  //  4. multi-word indentifiers
  ////////////////////////////////////////////////////////////////////////////////
  class Talker(name: String) {
    infix def `and then said`(gossip: String) = println(s"$name said \"$gossip\"")
  }

  val fan = new Talker("Liang Fan")
  fan `and then said` "I love scala3 very much"

  /* example: HTTP libraries */
  object `Content-Type` {
    val `application/json` = "application/JSON"
  }


  ////////////////////////////////////////////////////////////////////////////////
  //  5. infix types
  ////////////////////////////////////////////////////////////////////////////////
  @scala.annotation.targetName("Arrow")   /* for more readable bytecode + Java interop */
  infix class -->[A, B]

  val compositeType: -->[Int, String] = new -->[Int, String]
  val compositeTypeV2: Int --> String = new -->[Int, String]


  ////////////////////////////////////////////////////////////////////////////////
  //  6. update()
  ////////////////////////////////////////////////////////////////////////////////
  val anArray = Array(0, 1, 2, 3)
  anArray.update(1, 10)
  anArray(2) = 20
  println(anArray.mkString(", "))


  ////////////////////////////////////////////////////////////////////////////////
  //  7. mutable fields
  ////////////////////////////////////////////////////////////////////////////////
  class Mutable {
    private var internalState: Int = 0

    def member = internalState  //getter
    def member_=(value: Int): Unit = this.internalState = value //setter
  }

  val aMutableContainer = new Mutable
  aMutableContainer.member = 42   //to call setter
  println(aMutableContainer.member)


  ////////////////////////////////////////////////////////////////////////////////
  //  8. variable arguments (varargs)
  ////////////////////////////////////////////////////////////////////////////////
  def methodWithVarargs(args: Int*) = {
    println(args.mkString("[ ", ", ", " ]"))
    args.length
  }

  methodWithVarargs()
  methodWithVarargs(1)
  methodWithVarargs(2, 3)
  methodWithVarargs(List(10, 20, 30)*)


  def main(args: Array[String]): Unit = {

  }
}
