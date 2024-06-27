package com.rockthejvm.cats.part3datamanipulation

import java.util.concurrent.Executors
import scala.concurrent.{ExecutionContext, Future}

object Writers {

  import cats.data.Writer
  // 1 - define them at the start
  val aWriter: Writer[List[String], Int] = Writer(List("Started something"), 45)
  // 2 - manipulate them with pure FP
  val anIncreasedWriter = aWriter.map(_ + 1) // value increases, logs stay the same

  val aLogsWriter = aWriter.mapWritten(
    _ :+ "found something interesting"
  ) // value stays the same, logs change

  val aWriterWithBoth = aWriter.bimap(
    _ :+ "found something interesting",
    _ + 1
  ) // both value and logs change

  val aWriterWithBoth2 = aWriter.mapBoth { (logs, value) =>
    (logs :+ "found something interesting", value + 1)
  }

  // flatMap
  import cats.instances.vector._ // imports a Semigroup[Vector]
  val writerA = Writer(Vector("Log A1", "Log A2"), 10)
  val writerB = Writer(Vector("Log B1"), 40)

  val compositeWriter = for {
    va <- writerA
    vb <- writerB
  } yield va + vb

  // reset the logs
  import cats.instances.list._ // an implicit Monoid[List[Int]]
  val anEmptyWriter = aWriter.reset // clear the logs, keep the value

  // 3 - dump either the value or the logs
  val desiredValue = aWriter.value
  val logs         = aWriter.written
  val (l, v)       = aWriter.run

  // TODO 1: rewrite a function which "prints" things with writers
  def countAndSay(n: Int): Unit =
    if (n <= 0) println("starting!")
    else {
      countAndSay(n - 1)
      println(n)
    }

  /**
      在递归中，"返回"（或"回溯"）指的是递归函数完成其调用的递归实例并返回到上一层递归调用的过程。
      在递归函数中，每当一个递归调用被执行时，当前函数的执行环境（包括局部变量、参数等）会被压入调用栈。
      当这个递归调用完成后，它会返回到调用它的那一层递归，同时恢复那一层的执行环境，继续执行下一条指令。
                            ^^^^^^^^^^^^^^^^^^^  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

      让我们以 countAndSay 函数为例，来具体说明 "返回" 发生在哪里：

      考虑 countAndSay(3) 的调用过程（假设 println(n) 在 countAndSay(n - 1) 之后）：

      countAndSay(3) 开始执行，首先调用 countAndSay(2)。
      要执行 countAndSay(2)，它首先调用 countAndSay(1)。
      要执行 countAndSay(1)，它首先调用 countAndSay(0)。
      countAndSay(0) 是基本情况，打印 "starting!" 并且准备返回到它的调用者，即 countAndSay(1)。
      在第4步中，当 countAndSay(0) 完成后，它 "返回" 到 countAndSay(1)。这意味着执行流程回到 countAndSay(1) 调用 countAndSay(0) 之后的位置。由于 countAndSay(0) 是在 countAndSay(1) 中被调用的，所以现在 countAndSay(1) 继续执行 countAndSay(0) 之后的代码，即 println(1)。

      这个 "返回" 过程逐层向上，直到最初的调用：

      从 countAndSay(0) 返回到 countAndSay(1)：countAndSay(1) 执行 println(1)。
      然后从 countAndSay(1) 返回到 countAndSay(2)：countAndSay(2) 执行 println(2)。
      然后从 countAndSay(2) 返回到 countAndSay(3)：countAndSay(3) 执行 println(3)。
      在每一层的递归中，"返回" 实际上是回到了该层递归调用之后的代码执行点。这个过程允许每层递归完成它之后的操作，这也是为什么递归能够在达到基本条件后，以相反的顺序执行操作的原因。
                      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  */

  def countAndLog(n: Int): Writer[Vector[String], Int] =
    if (n <= 0) Writer(Vector("starting!"), 0)
    else countAndLog(n - 1).flatMap(_ => Writer(Vector(s"$n"), n))

  // Benefit #1: we work with pure FP

  // TODO 2: rewrite this method with writers
  def naiveSum(n: Int): Int =
    if (n <= 0) 0
    else {
      println(s"Now at $n")
      val lowerSum = naiveSum(n - 1)
      println(s"Computed sum(${n - 1}) = $lowerSum")
      lowerSum + n
    }

  def sumWithLogs(n: Int): Writer[Vector[String], Int] =
    if (n <= 0) Writer(Vector(), 0)
    else for {
      _        <- Writer(Vector(s"Now at $n"), n)
      lowerSum <- sumWithLogs(n - 1)
      _        <- Writer(Vector(s"Computed sum(${n - 1}) = $lowerSum"), n)
    } yield lowerSum + n

  // Benefit #2: Writers can keep logs separate on multiple threads

  implicit val ec: ExecutionContext =
    ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(4))

  def main(args: Array[String]): Unit = {
    // println(compositeWriter.run)
    // ex 1
    // countAndSay(10)
    // countAndLog(10).written.foreach(println)

    // naiveSum(10)
    // sumWithLogs(10).written.foreach(println)

    // ex 2
    Future(naiveSum(100)).foreach(println)
    Future(naiveSum(100)).foreach(println)

    // val sumFuture1 = Future(sumWithLogs(100))
    // val sumFuture2 = Future(sumWithLogs(100))
    // val logs1      = sumFuture1.map(_.written) // logs from thread 1
    // val logs2      = sumFuture2.map(_.written) // logs from thread 2
  }

}
