package com.rockthejvm.`scala3advanced`.practice

object Recursion {

  def sumUntil(n: Int): Int = {
    if n <= 0 then 0
    else n + sumUntil(n - 1)
  }

  def sumUntilV2(n: Int): Int = {
    /**
      * work process
      *  - sumUntilTailrec(10, 0)
      *  - sumUntilTailrec(9, 0 + 10)
      *  - sumUntilTailrec(8, 0 + 10 + 9)
      *  - sumUntilTailrec(7, 0 + 10 + 9 + 8)
      *  -  ...sumUntilTailrec(0, 0 + 10 + ... + 1)  => 0 + 10 + 9 + ... + 1
      * @param end
      * @param acc
      * @return
      */
    @scala.annotation.tailrec
    def sumUntilTailrec(end: Int, acc: Int): Int = {
      if end <= 0 then acc
      else sumUntilTailrec(end - 1, acc + end)
    }

    sumUntilTailrec(n, 0)
  }

  def main(args: Array[String]): Unit = {
    println(sumUntil(100))
    //println(sumUntil(100000))
    println(sumUntilV2(100000))
  }
}
