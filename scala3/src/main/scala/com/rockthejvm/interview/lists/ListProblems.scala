package com.rockthejvm.interview.lists

import scala.annotation.tailrec
import org.slf4j.{Logger, LoggerFactory}

sealed abstract class RList[+T] {
  def head: T

  def tail: RList[T]

  def isEmpty: Boolean

  def ::[S >: T](elem: S): RList[S] = new Cons(elem, this)

  /**
    * easy difficulty problems
    */
  // get element at a given index
  def apply(index: Int): T

  // the size of the list
  def length: Int

  // reverse the list
  def reverse: RList[T]

  // concatenate another list to this one
  infix def ++[S >: T](anotherList: RList[S]): RList[S]

  // remove an element at a given index, return a new list
  def removeAt(index: Int): RList[T]

  // the big 3
  def map[S](f: T => S): RList[S]

  def flatMap[S](f: T => RList[S]): RList[S]

  def filter(f: T => Boolean): RList[T]


  /**
    * medium difficulty problems
    */
  // run-length encoding
  def rle: RList[(T, Int)]

  // duplicate each elment k times
  def duplicateEach(k: Int): RList[T]

  // rotation by a number of positions to the left
  def rotate(k: Int): RList[T]
}

case object RNil extends RList[Nothing] {
  override def head: Nothing = throw new NoSuchElementException

  override def tail: RList[Nothing] = throw new NoSuchElementException

  override def isEmpty: Boolean = true

  override def toString: String = "[]"

  override def apply(index: Int): Nothing = throw new NoSuchElementException

  override def length: Int = 0

  override def reverse: RList[Nothing] = RNil

  override infix def ++[S >: Nothing](anotherList: RList[S]): RList[S] = anotherList

  override def removeAt(index: Int): RList[Nothing] = RNil

  override def map[S](f: Nothing => S): RList[S] = RNil

  override def flatMap[S](f: Nothing => RList[S]): RList[S] = RNil

  override def filter(f: Nothing => Boolean): RList[Nothing] = RNil

  override def rle: RList[(Nothing, Int)] = RNil

  override def duplicateEach(k: Int): RList[Nothing] = RNil

  override def rotate(k: Int): RList[Nothing] = RNil
}

case class Cons[+T](override val head: T, override val tail: RList[T]) extends RList[T] {
  override def isEmpty: Boolean = false

  override def toString: String = {
    @scala.annotation.tailrec
    def toStringHelper(remaining: RList[T], result: String): String = {
      if (remaining.isEmpty) result
      else if (remaining.tail.isEmpty) s"$result${remaining.head}"
      else toStringHelper(remaining.tail, s"$result${remaining.head}, ")
    }

    "[" + toStringHelper(this, "") + "]"
  }

  override def apply(index: Int): T = {
    /**
      * 在设计tailrec类型的函数时候，需要考虑两点：
      *  - 哪些关键元素是一直在变化的，这些元素都要作为变量参数来考虑
      *  - 边界/退出条件是啥
      *
      * @param remaining  -- 未搜索过的剩余列表
      * @param currentIdx -- 遍历过了的元素下标
      * @return
      */
    @scala.annotation.tailrec
    def applyHelper(remaining: RList[T], currentIdx: Int): T =
      if (currentIdx == index) remaining.head
      else applyHelper(remaining.tail, currentIdx + 1)

    if (index < 0) throw new Exception(s"index should be >= 0, but got [ $index ]")
    else applyHelper(this, 0)
  }

  override def length: Int = {
    @scala.annotation.tailrec
    def lenHelper(remaining: RList[T], counter: Int): Int =
      if (remaining.isEmpty) counter
      else lenHelper(remaining.tail, counter + 1)

    lenHelper(this, 0)
  }

  override def reverse: RList[T] = {
    @tailrec
    def reverseHelper(remaining: RList[T], newList: RList[T]): RList[T] =
      if (remaining.isEmpty) then newList
      else reverseHelper(remaining.tail, remaining.head :: newList)

    reverseHelper(this, RNil)
  }

  // stack overflow
  //override infix def ++[S >: T](anotherList: RList[S]): RList[S] = this.head :: (this.tail ++ anotherList)

  override infix def ++[S >: T](anotherList: RList[S]): RList[S] = {
    @tailrec
    def concatHelper(remaining: RList[T], result: RList[S]): RList[S] = remaining match {
      case RNil => result
      case Cons(head, tail) => concatHelper(tail, head :: result)
    }

    concatHelper(this.reverse, anotherList)
  }

  override def removeAt(index: Int): RList[T] = {
    @tailrec
    def removeHelper(currentIndex: Int, remaining: RList[T], traversed: RList[T]): RList[T] =
      if (remaining.isEmpty) then this
      else if (currentIndex == index) then traversed.reverse ++ remaining.tail
      else removeHelper(currentIndex + 1, remaining.tail, remaining.head :: traversed)

    if (index < 0) then this
    else removeHelper(0, this, RNil)
  }

  override def map[S](f: T => S): RList[S] = {
    @tailrec
    def mapHelper(remaining: RList[T], proccessed: RList[S]): RList[S] = remaining match {
      case RNil => proccessed.reverse
      case Cons(x, xs) => mapHelper(xs, f(x) :: proccessed)
    }

    mapHelper(this, RNil)
  }

  override def flatMap[S](f: T => RList[S]): RList[S] = {
    @tailrec
    def flatmapHelper(remaining: RList[T], proccessed: RList[S]): RList[S] = remaining match {
      //      case RNil => proccessed
      //      case Cons(x, xs) => flatmapHelper(xs, proccessed ++ f(x))

      // this one more better
      case RNil => proccessed.reverse
      case Cons(x, xs) => flatmapHelper(xs, f(x).reverse ++ proccessed)
    }

    flatmapHelper(this, RNil)
  }

  override def filter(f: T => Boolean): RList[T] = {
    @tailrec
    def filterHelper(remaining: RList[T], proccessed: RList[T]): RList[T] = remaining match {
      case RNil => proccessed.reverse
      case Cons(x, xs) if f(x) => filterHelper(xs, x :: proccessed)
      case Cons(_, xs) => filterHelper(xs, proccessed)
    }

    filterHelper(this, RNil)
  }

  override def rle: RList[(T, Int)] = {
    def rleHelper(remaining: RList[T], currrentTuple: (T, Int), result: RList[(T, Int)]): RList[(T, Int)] = remaining match {
      case RNil => currrentTuple :: result
      case Cons(x, xs) if (x == currrentTuple._1) => rleHelper(xs, (x, currrentTuple._2 + 1), result)
      case Cons(a, as) => rleHelper(as, (a, 1), currrentTuple :: result)
    }

    rleHelper(this.tail, (this.head, 1), RNil).reverse
  }

  override def duplicateEach(k: Int): RList[T] = {
    def dupHelper(remaining: RList[T], nTimes: Int, result: RList[T]): RList[T] = remaining match {
      case RNil => result
      case Cons(x, xs) if (nTimes == 0) => dupHelper(xs, k, result)
      case Cons(a, as) => dupHelper(remaining, nTimes - 1, a :: result)
    }

    dupHelper(this, k, RNil).reverse
  }

  override def rotate(k: Int): RList[T] = {
    require(k >= 0)

    @tailrec
    def rotateHelper(remaining: RList[T], numLeft: Int, buffer: RList[T]): RList[T] = remaining match {
      case RNil => buffer.reverse
      case Cons(x, xs) if numLeft == 0 => remaining ++ buffer.reverse
      case Cons(y, ys) => rotateHelper(ys, numLeft - 1, y :: buffer)
    }

    rotateHelper(this, k, RNil)
    //    if (k <= 0 || k >= this.length) this
    //    else rotateHelper(this, k, RNil)
  }
}

object RList {
  def fromIterable[T](iterable: Iterable[T]) = {
    def convertHelper(remaining: Iterable[T], result: RList[T]): RList[T] =
      if (remaining.isEmpty) then result
      else convertHelper(remaining.tail, remaining.head :: result)

    convertHelper(iterable, RNil).reverse
  }
}

object ListProblems extends App {
  private val logger: Logger = LoggerFactory.getLogger(this.getClass)

  def testEasy = {
    val aSmallList = 1 :: 2 :: 3 :: RNil
    val largeList = RList.fromIterable(1 to 20000)
    val anotherList = 10 :: 20 :: 30 :: RNil
    println(aSmallList.toString)
    println(largeList)

    println(aSmallList.apply(0))
    println(aSmallList.apply(1))
    println(aSmallList.apply(2))
    //println(aSmallList.apply(3))
    println(largeList.apply(9999))

    println(aSmallList.tail.length)
    println(largeList.length)

    println(aSmallList.reverse)
    println(largeList.reverse)

    println(aSmallList ++ anotherList)

    println(aSmallList.removeAt(1))
    println(aSmallList.removeAt(2))
    println(RNil.removeAt(1))
    println(aSmallList.removeAt(100))

    println(largeList.map(x => x * 5))

    val time = System.currentTimeMillis()
    largeList.flatMap(x => x :: (2 * x) :: RNil)
    largeList.map(x => x * 2)
    println(System.currentTimeMillis() - time)

    println(largeList.filter(x => x % 2 == 0))
  }

  def testMedium = {
    val simpleList = 1 :: 1 :: 2 :: 2 :: 3 :: 4 :: 4 :: 5 :: RNil
    val smallList = 1 :: 2 :: 3 :: RNil
    val tens = RList.fromIterable(1 to 10)

    //    println(simpleList.rle)
    //    println(RNil.rle)
    //    println((1 :: RNil).rle)
    //
    //    println(smallList.duplicateEach(3))
    //    println(smallList.duplicateEach(0))
    //    println(smallList.duplicateEach(1))
    //    println(smallList.duplicateEach(2))

    println(RNil.rotate(1))
    //println(smallList.rotate(-1))
    println(smallList.rotate(0))
    println(smallList.rotate(1))
    println(smallList.rotate(2))
    println(smallList.rotate(3))
    println(smallList.rotate(4))

    for (i <- 0 to 20) {
      println(s"rotate $i elemnts: ${tens.rotate(i)}")
    }
  }

  def quickSort(list: List[Int]): List[Int] = list match {
    case Nil => Nil
    case head :: tail =>
      val smaller = tail.filter(_ < head)
      val bigger = tail.filter(_ >= head)
      quickSort(smaller) ++ List(head) ++ quickSort(bigger)
  }

  extension[T] (list: List[T]) {
    def makeString: String = list.mkString("[", ", ", "]")
  }

  /***
    *  - 先会分拆到最细粒度，left和right最多1个元素的粒度，然后在 [branch 4] 中有序
    *  - 使左侧有序
    *  - 使右侧有序
    *  - 两边有序的再合并一次
    */
  private def mergeSort(items: List[Int]): List[Int] = {
    logger.error(s"[branch 0] items: ${items.makeString}")

    @scala.annotation.tailrec
    def merge(left: List[Int], right: List[Int], acc: List[Int]): List[Int] = (left, right) match {
      case (Nil, Nil) => {
        logger.warn(s"[branch 1] acc: $acc")
        acc
      }
      case (Nil, head :: tail) => {
        logger.warn(s"[branch 2] left: ${left.makeString}; right: ${right.makeString}; acc: ${acc.makeString}")
        merge(Nil, tail, head :: acc)
      }
      case (head :: tail, Nil) => {
        logger.warn(s"[branch 3] left: ${left.makeString}; right: ${right.makeString}; acc: ${acc.makeString}")
        merge(tail, Nil, head :: acc)
      }
      case (leftHead :: leftTail, rightHead :: rightTail) => {
        logger.warn(s"[branch 4] left: ${left.makeString}; right: ${right.makeString}; acc: ${acc.makeString}")
        if (leftHead < rightHead) merge(leftTail, right, leftHead :: acc)
        else merge(left, rightTail, rightHead :: acc)
      }
    }

    val (leftHalf, rightHalf) = items.splitAt(items.length / 2)
    if (items.length <= 1) items
    //会先使左边有序，再让右边有序，再来合并一次; reverse是必须的(为啥呢？还没搞清楚)
    else merge(mergeSort(leftHalf), mergeSort(rightHalf), Nil).reverse
  }

  private def mergeSortGeneric[T: Ordering](items: List[T]): List[T] = {
    @scala.annotation.tailrec
    def merge(left: List[T], right: List[T], acc: List[T]): List[T] = (left, right) match
      case (Nil, Nil) => acc
      case (Nil, rightHead :: rightTail) => merge(Nil, rightTail, rightHead :: acc)
      case (leftHead :: leftTail, Nil) => merge(leftTail, Nil, leftHead :: acc)
      case (leftHead :: leftTail, rightHead :: rightTail) =>
        if (Ordering[T].lt(leftHead, rightHead)) merge(leftTail, right, leftHead :: acc)
        else merge(left, rightTail, rightHead :: acc)

    if (items.length <= 1) items
    else
      val (part1, part2) = items.splitAt(items.length / 2)
      merge(mergeSortGeneric(part1), mergeSortGeneric(part2), Nil).reverse
  }

  def insertSort(list: List[Int]): List[Int] = {
    def insert(x: Int, sortedList: List[Int]): List[Int] = sortedList match {
      case Nil => List(x)
      case head :: tail =>
        if (x < head) x :: insert(head, tail)
        else head :: insert(x, tail)
    }

    list.foldLeft(List.empty[Int])((acc, x) => insert(x, acc))
  }

  val listLit = List(2, 3, 7, 4, 1, 5, 9, 8, 6)
  println(quickSort(List(2, 3, 7, 4, 1, 5, 9, 8, 6)))

  println(insertSort(List(2, 3, 7, 4, 1, 5, 9, 8, 6)))

  /**
    * [2,3] => [3, 2]
    * [7, 4, 1] => [7] [4, 1]
    */
  println(mergeSort(List(2, 7, 4, 1)))

  println(mergeSortGeneric(listLit))

  //testMedium
}
