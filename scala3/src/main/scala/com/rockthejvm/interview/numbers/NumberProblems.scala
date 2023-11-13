package com.rockthejvm.interview.numbers

import com.rockthejvm._

object NumberProblems extends App {

  /***
    * 判断质数
    * @param n
    * @return
    */
  def isPrime(n: Int): Boolean = {
    @scala.annotation.tailrec
    def primeTailrec(divisor: Int): Boolean = {
      //logger.debug(s"current divisor: $divisor")
      if (divisor > Math.sqrt(n)) true  //临界条件是平方根，超过平方根，说明所有的可能性都尝试完毕了
      else if (n % divisor == 0) false  //能被某个除数整除那就不是质数
      else primeTailrec(divisor + 1)    //继续加大除数继续试
    }

    if (n <= 1) false
    else primeTailrec(2)
  }

  println(isPrime(20))
  println(isPrime(1927489))

  /***
    * 大数之和
    * 0 7 2 4 5 7
    * 0 0 0 3 9 2
    */
  private def largeSum(num1: String, num2: String): String = {
    val len1 = num1.length
    val len2 = num2.length
    val maxLen = Math.max(num1.length, num2.length)

    val part1 = s"${"0" * (maxLen - len1 + 1)}$num1"
    val part2 = s"${"0" * (maxLen - len2 + 1)}$num2"
    logger.info(s"part1: $part1; part2: $part2")

    val result = Array.fill(maxLen + 1)(0)
    val highDigit = Array.fill(maxLen + 1)(0)

    (1 until maxLen + 1).reverse foreach { i =>
      //char 的 toInt方法小心使用
      val tmp = part1.slice(i, i + 1).toInt + part2.slice(i, i + 1).toInt + highDigit(i)
      logger.info(s"tmp: $tmp")
      if (tmp < 10)
        result(i) = tmp
      else
        result(i) = tmp - 10
        highDigit(i - 1) = 1
    }

    //最大的位有可能进1了
    result(0) = highDigit(0)

    logger.warn(s"result: ${result.mkString("[", ", ", "]")}; digit: ${highDigit.mkString("[", ", ", "]")}")
    result.dropWhile(i => i == 0).mkString
  }

  println(largeSum("72457", "392"))
  println(largeSum("999999999", "999999999"))


  /***
    * 数组组合成最大的数字
    * given a list of non-negative integers, arrange of them such that they form the largest number
    * [23, 4, 1, 0, 2, 456]
    */
  private def largestNumber(numbers: List[Int]): String = {
    implicit val newOrdering: Ordering[Int] = Ordering.fromLessThan { (a, b) =>
      val aStr = a.toString
      val bStr = b.toString

      (aStr + bStr).compareTo(bStr + aStr) >= 0
    }

    val largest = numbers.sorted(newOrdering)
    logger.warn(largest.mkString("[", ", ", "]"))

    if (largest.head == 0) "0"
    else largest.mkString
  }

  println(largestNumber(List(23, 4, 1, 0, 2, 456)))
  println(largestNumber(List(0, 0, 1)))
  println(largestNumber(List(0, 0, 0)))



}
