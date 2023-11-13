package com.rockthejvm.interview.strings

import scala.collection.mutable

object StringProblem extends App {

  //最长不重复子字符串 pwwkew => 3
  private def longestSubstring(str: String): Int = {
    // 双指针与索引Map
    var maxLength = 0
    var start = 0
    var end = 0

    val indexMap = mutable.HashMap.empty[Char, Int]

    str.foreach { char =>
      if (indexMap.contains(char))
        start = math.max(indexMap(char), start) // 更新 start

      end += 1 // 更新 end
      indexMap(char) = end
      maxLength = math.max(end - start, maxLength) // 更新 maxLength
      println(s"char: $char start: $start end: $end max: $maxLength arr: ${str.slice(start, end).mkString("")}")
    }
    maxLength
  }

  println(longestSubstring("aabb"))
  println(longestSubstring("pwwkew"))

  //最长回文子字符串  aba  abcba
  def palindromic(str: String): String = {
    var length = 1
    // 从每个 mid 开始向两边扩散
    var maxLeft = 0 // 起点
    var maxRight = 0 // 终点
    var maxLength = 0 // 最长回文串长度

    val arr = str.toCharArray

    arr.indices foreach { mid =>

      var left = mid - 1
      var right = mid + 1

      // 向左侧扩展
      while (left >= 0 && arr(left).equals(arr(mid))) {
        left -= 1
        length += 1
      }

      // 向右侧扩展
      while (right <= arr.length - 1 && arr(right) == arr(mid)) {
        right += 1
        length += 1
      }

      // 向两侧扩展
      while (left >= 0 && right <= arr.length - 1 && arr(left) == arr(right)) {
        left -= 1
        right += 1
        length += 2
      }

      // 更新
      if (length > maxLength) {
        maxLeft = left
        maxRight = right
        maxLength = length
      }

      length = 1
    }

    println(s"$maxLeft, $maxRight, $maxLength")

    str.substring(maxLeft + 1, maxRight)
  }
}
