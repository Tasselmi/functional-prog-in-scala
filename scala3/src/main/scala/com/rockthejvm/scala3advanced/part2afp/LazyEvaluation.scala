package com.rockthejvm.`scala3advanced`.part2afp

import scala.collection.WithFilter

object LazyEvaluation {

  ////////////////////////////////////////////////////////////////////////////////
  //  1. example one: call by need
  ////////////////////////////////////////////////////////////////////////////////
  lazy val x: Int = {
    println("hello")
    42
  }

  def byNameMethod(n: => Int): Int =
    n + n + n + 1

  def retrieveMagicValue() =
    println("waiting...")
    Thread.sleep(1000L)
    42

  def demoByName(): Unit =
    println(byNameMethod(retrieveMagicValue()))

  demoByName()  //retrieveMagicValue will be invoked 3 times

  println("----------------------- splitting line ----------------------- ")

  // lazy delays the evaluation of a value until the first use
  // evaluation occurs once

  // call by need = call by nmae + lazy values
  def byNeedMethod(n: => Int): Int =
    lazy val lazyN = n  /* memoization */
    lazyN + lazyN + lazyN + 1

  def demoByNeed(): Unit =
    println(byNeedMethod(retrieveMagicValue()))

  demoByNeed()


  ////////////////////////////////////////////////////////////////////////////////
  //  2. example two: withFilter
  ////////////////////////////////////////////////////////////////////////////////
  def greaterThan20(i: Int): Boolean =
    println(s"$i is greater than 20 ?")
    i > 20

  def lessThan30(i: Int): Boolean =
    println(s"$i is less than 30 ?")
    i < 30

  val numnbers = List(1, 25, 40, 5, 23)

  def demoFilter(): Unit =
    val gt20 = numnbers.filter(greaterThan20)
    val lt30 = gt20.filter(lessThan30)
    println(lt30)

  demoFilter()

  println("----------------------- splitting line ----------------------- ")

  def demoWithFilter(): Unit =
    val gt20: WithFilter[Int, List] = numnbers.withFilter(greaterThan20)
    val lt30: WithFilter[Int, List] = gt20.withFilter(lessThan30)
    println(lt30.map(identity))

  demoWithFilter()

  println("----------------------- splitting line ----------------------- ")

  def demoForComprehension(): Unit =
    val forComp = for {
      n <- numnbers if greaterThan20(n) && lessThan30(n)
    } yield n
    println(forComp)

  demoForComprehension()

  def main(args: Array[String]): Unit = {

  }
}
