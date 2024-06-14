package com.rockthejvm.`scala3advanced`.part2afp

/**
  * partial functions != partially applied functions
  */
object CurryingPafs {  //currying and partially applied functions

  ////////////////////////////////////////////////////////////////////////////////
  //  1. currying
  ////////////////////////////////////////////////////////////////////////////////
  val superAdder: Int => Int => Int = x => y => x + y

  val add3: Int => Int = superAdder(3)
  val eight = add3(5)
  val eightV2 = superAdder(3)(5)


  ////////////////////////////////////////////////////////////////////////////////
  //  2. curried methods
  ////////////////////////////////////////////////////////////////////////////////
  def curriedAdder(x: Int)(y: Int): Int =
    x + y


  ////////////////////////////////////////////////////////////////////////////////
  //  3. methods != function values/instances
  ////////////////////////////////////////////////////////////////////////////////
  /**
    * converting methods to functions = eta-expansion
    */
  val add4 = curriedAdder(4)  //eta-expansion -- eta 希腊字母第七个 H
  val nine = add4(5)

  def increment(x: Int): Int = x + 1
  val aList = List(1, 2, 3)
  val incrementedList = aList.map(increment) //eta-expansion


  ////////////////////////////////////////////////////////////////////////////////
  //  4. underscores are powerful: allow you to decide the shopes of lambdas obtained from methods
  ////////////////////////////////////////////////////////////////////////////////
  def concatenator(a: String, b: String, c: String) = a + b + c

  val insertName: String => String = concatenator(
    "Hello, my name is ",
    _: String,
    ". Nice to meet you."
  )
  val fanGreeting = insertName("Liang Fan")
  println(fanGreeting)

  val fillInBlank: (String, String) => String = concatenator(_: String, "LiangFan", _: String)
  val fanGreetingV2 = fillInBlank("Hi, ", ", How are you?")
  println(fanGreetingV2)


  ////////////////////////////////////////////////////////////////////////////////
  //  5. exercises to use currying
  ////////////////////////////////////////////////////////////////////////////////

  /**
    * Exercises
    * @param args
    */
  val simpleAddFunction = (x: Int, y: Int) => x + y
  def simpleAddMethod(x: Int, y: Int) = x + y
  def curriedAddMethod(x: Int)(y: Int) = x + y

  // 5.1 obtain an add7 function: x => x + 7 out of these 3 definitions
  val add7V1 = (x: Int) => simpleAddFunction(x, 7)
  val add7V2 = (x: Int) => simpleAddMethod(x, 7)
  val add7V3 = (x: Int) => curriedAddMethod(x)(7)
  val add7V4 = curriedAddMethod(7)
  val add7V5 = simpleAddMethod(7, _)
  val add7V6 = simpleAddFunction.curried(7)
  val add7V7 = simpleAddMethod.curried(7)

  // 5.2 process a list of numbers and return their string representations under different formats
  //  step 1: create a curried formatting method with a formatting string and a value
  def curriedFormatter(fmtExpr: String)(number: Double): String = fmtExpr.format(number)
  //  step 2: process a list of numbers with various formats
  val someDecimals = List(Math.PI, Math.E, 1, 9.8, 1.3e-12)
  println(someDecimals.map(curriedFormatter("%4.2f")))
  println(someDecimals.map(curriedFormatter("%8.6f")))
  println(someDecimals.map(curriedFormatter("%12.10f")))
  println(someDecimals.map(curriedFormatter("%16.14f")))
  println(someDecimals.map(curriedFormatter("%20.18f")))


  ////////////////////////////////////////////////////////////////////////////////
  //  6. methods vs functions && by-name vs 0-lambdas
  ////////////////////////////////////////////////////////////////////////////////
  def byName(n: => Int) = n + 1
  def byLambda(f: () => Int) = f() + 1

  def method: Int = 42
  def parenMethod(): Int = 42

  byName(23)
  byName(method)  //eta-expansion? NO - method is invoked here
  byName(parenMethod())
  //byName(parenMethod) //not ok
  byName((() => 42)())
  //byName(() => 42) //not ok, bcs byName not accept function instance

  //byLambda(23)  //not ok
  //byLambda(method)  //eta-expansion is not possible
  byLambda(parenMethod) //eta-expansion is done
  byLambda(() => 42)
  byLambda(() => parenMethod()) /* () represents invoke a function */


  ////////////////////////////////////////////////////////////////////////////////
  //  7. call by name  VS  call by value
  ////////////////////////////////////////////////////////////////////////////////
  //call by value = arguments are evaluated before function invocation
  def aFunction(arg: Int): Int = arg + 1
  val aComputation = aFunction(23 + 76)

  //call by name = arguments are passed literally, evaluated at every reference
  def aByNameFunc(arg: => Int): Int = arg + 1
  val anotherComputation = aByNameFunc(23 + 76)

  def printTwiceByValue(x: Long): Unit =
    println("by value: " + x)
    Thread.sleep(1000L)
    println("by value: " + x)

  /**
    * there are 2 differences:
    *  - delayed evaluation
    *  - evaluated every time it is used
    */
  def printTwiceByName(x: => Long): Unit =
    println("by  name: " + x)
    Thread.sleep(1000L)
    println("by  name: " + x)

  printTwiceByValue(System.nanoTime())
  printTwiceByName(System.nanoTime())

  
  def infinite(): Int = 1 + infinite()
  def printFirst(x: Int, y: => Int) = println(x)
  //printFirst(infinite(), 6) //java.lang.StackOverflowError
  printFirst(6, infinite())


  def main(args: Array[String]): Unit = {

  }
}
