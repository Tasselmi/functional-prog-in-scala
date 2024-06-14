package com.rockthejvm.`scala3advanced`.part3aync

import scala.collection.parallel.immutable.*
import scala.collection.parallel.CollectionConverters.*
import org.apache.commons.lang3.time.StopWatch

object ParallelCollections {

  ////////////////////////////////////////////////////////////////////////////////
  //  1. build a parallel collection
  ////////////////////////////////////////////////////////////////////////////////
  val aList = (1 to 5).toList
  val parList: ParSeq[Int] = aList.par

  parList.map(_ + 1).foreach(println)

  val parVec = ParVector[Int](1, 2, 3, 4)


  ////////////////////////////////////////////////////////////////////////////////
  //  2. performance comparison
  ////////////////////////////////////////////////////////////////////////////////
  def measure[T](computation: => T): Long = {
    val stopWatch = StopWatch.createStarted()
    computation
    stopWatch.stop()
    stopWatch.getTime(java.util.concurrent.TimeUnit.MILLISECONDS)
  }

  def performanceCompare(max: Int): Unit = {
    val bigList = (1 to max).toList
    println(s"a big list with $max elements created...")

    val serialTime = measure(bigList.map(_ + 1)).toString
    val parallelTime = measure(bigList.par.map(_ + 1)).toString

    println(s"[Serial  ]  ${serialTime}")
    println(s"[Parallel]  ${parallelTime.reverse.padTo(serialTime.length, ' ').reverse}")
    println("====================================")
  }


  ////////////////////////////////////////////////////////////////////////////////
  //  3. performance comparison
  ////////////////////////////////////////////////////////////////////////////////
  def demoUndifinedOrder(): Unit = {
    val list = (1 to 100).toList
    val reduction = list.reduce(_ - _)
    val parReduction = list.par.reduce(_ - _)

    println(s"sequential: $reduction, parallel: $parReduction")
  }

  //for associative operations, the result is deterministic
  def demoDefinedOrder(): Unit = {
    val strings = "I love parallel collections but I must be careful".split(" ")
    val reduction = strings.reduce(_ + " " +  _)
    val parReduction = strings.par.reduce(_ + " " +  _)

    println(s"sequential: $reduction\nparallel:   $parReduction")
  }

  def demoRaceCondition(): Unit = {
    var sum = 0
    (1 to 100).par.foreach(i => sum += i)
    println(sum)
  }


  def main(args: Array[String]): Unit = {
//    performanceCompare(10000000)
//    performanceCompare(20000000)
//    performanceCompare(30000000)
    demoUndifinedOrder()
    demoDefinedOrder()
    demoRaceCondition()
  }
}
