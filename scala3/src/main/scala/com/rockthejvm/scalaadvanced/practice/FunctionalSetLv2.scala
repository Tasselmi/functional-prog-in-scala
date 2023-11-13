package com.rockthejvm.scalaadvanced.practice

object FunctionalSetLv2 {

  abstract class FuncSet[A] extends (A => Boolean) {
    //main api
    def contains(elem: A): Boolean
    def apply(elem: A) = contains(elem)

    infix def +(elem: A): FuncSet[A]
    infix def ++(otherSet: FuncSet[A]): FuncSet[A]

    //classics
    def map[B](f: A => B): FuncSet[B]
    def flatMap[B](f: A => FuncSet[B]): FuncSet[B]
    def filter(f: A => Boolean): FuncSet[A]
    def foreach(f: A => Unit): Unit

    //utilities
    infix def -(elem: A): FuncSet[A]
    infix def --(otherSet: FuncSet[A]): FuncSet[A]
    infix def &(otherSet: FuncSet[A]): FuncSet[A]

    //"negation" == all the elements of type A except the elments in this set
    def unary_! : FuncSet[A] =
      new PbSet[A](x => !contains(x))
  }


  /**
    * example { x in NaturalNumber | x % 2 == 0 }
    * property-based set
    * @param property
    * @tparam A
    */
  class PbSet[A](property: A => Boolean) extends FuncSet[A] {

    override def contains(elem: A): Boolean = property(elem)

    override infix def +(elem: A): FuncSet[A] =
      new PbSet[A](x => x == elem || property(x))

    override infix def ++(otherSet: FuncSet[A]): FuncSet[A] =
      new PbSet[A](x => property(x) || otherSet(x))

    override def map[B](f: A => B): FuncSet[B] =
      this.politelyFail()

    override def flatMap[B](f: A => FuncSet[B]): FuncSet[B] =
      this.politelyFail()

    override def filter(f: A => Boolean): FuncSet[A] =
      new PbSet[A](x => property(x) && f(x))

    override def foreach(f: A => Unit): Unit = this.politelyFail()

    override infix def -(elem: A): FuncSet[A] =
      this.filter(x => x != elem)

    override infix def --(otherSet: FuncSet[A]): FuncSet[A] =
      this.filter(!otherSet)

    override infix def &(otherSet: FuncSet[A]): FuncSet[A] =
      this.filter(otherSet)

    //extra utilities (internal)
    private def politelyFail() =
      throw new RuntimeException("I don't know if this set is iterable...")
  }


  case class Empty[A]() extends FuncSet[A] {  // PbSet(x => false)

    override def contains(elem: A): Boolean = false

    override infix def +(elem: A): FuncSet[A] = Cons(elem, this)

    override infix def ++(otherSet: FuncSet[A]): FuncSet[A] = otherSet

    override def map[B](f: A => B): FuncSet[B] = Empty()

    override def flatMap[B](f: A => FuncSet[B]): FuncSet[B] = Empty()

    override def filter(f: A => Boolean): FuncSet[A] = this

    override def foreach(f: A => Unit): Unit = ()

    override infix def -(elem: A): FuncSet[A] = this

    override infix def --(otherSet: FuncSet[A]): FuncSet[A] = this

    override infix def &(otherSet: FuncSet[A]): FuncSet[A] = this
  }


  case class Cons[A](head: A, tail: FuncSet[A]) extends FuncSet[A] {

    override def contains(elem: A): Boolean =
      elem == head || tail.contains(elem)

    override infix def +(elem: A): FuncSet[A] =
      if contains(elem) then this
      else Cons(elem, this)

    override infix def ++(otherSet: FuncSet[A]): FuncSet[A] = tail ++ otherSet + head

    override def map[B](f: A => B): FuncSet[B] = tail.map(f) + f(head)

    override def flatMap[B](f: A => FuncSet[B]): FuncSet[B] = tail.flatMap(f) ++ f(head)

    override def filter(f: A => Boolean): FuncSet[A] =
      if f(head) then tail.filter(f) + head
      else tail.filter(f)

    override def foreach(f: A => Unit): Unit =
      f(head)
      tail.foreach(f)

    override infix def -(elem: A): FuncSet[A] =
      if head == elem then tail
      else tail - elem + head

    override infix def --(otherSet: FuncSet[A]): FuncSet[A] =
      this.filter(x => !otherSet.apply(x))

    /**
      * this is fucking awesome, set itself is a function
      * @param otherSet
      * @return
      */
    override infix def &(otherSet: FuncSet[A]): FuncSet[A] = this.filter(otherSet)
  }


  object FuncSet {
    def apply[A](values: A*): FuncSet[A] =
      @scala.annotation.tailrec
      def buildSet(valueSeq: Seq[A], acc: FuncSet[A]): FuncSet[A] =
        if valueSeq.isEmpty then acc
        else buildSet(valueSeq.tail, acc + valueSeq.head)

      buildSet(values, Empty())
  }


  def main(args: Array[String]): Unit = {

    val first5 = FuncSet(1, 2, 3, 4, 5)
    println(first5.contains(5))
    println(first5(6))
    println((first5 + 10).contains(10))
    println(first5.map(_ * 2).contains(10))
    println(first5.map(_ % 2).contains(1))
    println(first5.flatMap(x => FuncSet(x, x + 1)).contains(7))

    println("-------------------------------------")

    println((first5 - 3).contains(3))
    val somNumbers = FuncSet(4, 5, 6, 7)
    println((first5 -- somNumbers).contains(4))
    println((first5 & somNumbers).contains(4))

    println("-------------------------------------")


    val naturals = new PbSet[Int](_ => true)
    println(naturals.contains(435678))
    println(!naturals.contains(0))
    println((!naturals + 1 + 2 + 3).contains(2))
    println(naturals.map(_ + 1))
  }
}
