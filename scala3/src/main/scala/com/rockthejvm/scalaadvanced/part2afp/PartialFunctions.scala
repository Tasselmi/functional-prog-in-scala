package com.rockthejvm.scalaadvanced.part2afp

import com.rockthejvm.scalaadvanced.part1as.AdvancedPatternMatching

object PartialFunctions {

  val aFunction: Int => Int = x => x + 1

  val aFussyFunction = (x: Int) =>
    if (x == 1) 42
    else if (x == 2) 56
    else if (x == 5) 999
    else throw new RuntimeException("no suitable cases possible")

  val aFussyFunctionV2 = (x: Int) => x match {
    case 1 => 42
    case 2 => 56
    case 5 => 999
  }


  ////////////////////////////////////////////////////////////////////////////////
  //  1. partial function and its utilities
  ////////////////////////////////////////////////////////////////////////////////
  val aPartialFunction: PartialFunction[Int, Int] = {
    case 1 => 42
    case 2 => 56
    case 5 => 999
  }

  println(aPartialFunction(1))
//  println(aPartialFunction(30))  //Caused by: scala.MatchError: 30 (of class java.lang.Integer)

  if (aPartialFunction.isDefinedAt(30))
    println(aPartialFunction(30))
  else
    println("not defined at 30")

  val liftedPartialFunc = aPartialFunction.lift  //tranfer a partial function to a total function
  println(liftedPartialFunc(5))
  println(liftedPartialFunc(30))

  val anotherPf: PartialFunction[Int, Int] = {
    case 45 => 886
  }
  val partialFuncChain = aPartialFunction.orElse(anotherPf)
  println(partialFuncChain(45))
  //println(partialFuncChain(999))


  ////////////////////////////////////////////////////////////////////////////////
  //  2. high-order functions accept partial functions as argument
  ////////////////////////////////////////////////////////////////////////////////
  val aList = List(1, 2, 3, 4)
  val changedList = aList.map(x => x match {
    case 1 => 11
    case 2 => 22
    case 3 => 33
    case 4 => 44
    case _ => 0
  })

  val changedListV2 = aList.map({ //because PartialFunction[-A, +B] extends Function1[A, B]
    case 1 => 11
    case 2 => 22
    case 3 => 33
    case 4 => 44
    case _ => 0
  })

  val changedListV3 = aList map { //sugar for methods with one argument
    case 1 => 11
    case 2 => 22
    case 3 => 33
    case 4 => 44
    case _ => 0
  }

  case class Person(name: String, age: Int)

  val someKids = List(
    Person("Alice", 3),
    Person("Bobbie", 5),
    Person("Jane", 4)
  )

  val kidsGrowingUp = someKids map {
    case Person(name, age) => Person(name, age + 1)
  }
  println(kidsGrowingUp.mkString("[ ", ", ", " ]"))


  ////////////////////////////////////////////////////////////////////////////////
  //  3. i
  ////////////////////////////////////////////////////////////////////////////////


  ////////////////////////////////////////////////////////////////////////////////
  //  4. i
  ////////////////////////////////////////////////////////////////////////////////


  ////////////////////////////////////////////////////////////////////////////////
  //  5. i
  ////////////////////////////////////////////////////////////////////////////////


  def main(args: Array[String]): Unit = {

  }
}
