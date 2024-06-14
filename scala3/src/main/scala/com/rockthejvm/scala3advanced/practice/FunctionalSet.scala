package com.rockthejvm.`scala3advanced`.practice

object FunctionalSet {

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
  }


  case class Empty[A]() extends FuncSet[A] {

    override def contains(elem: A): Boolean = false

    override infix def +(elem: A): FuncSet[A] = Cons(elem, this)

    override infix def ++(otherSet: FuncSet[A]): FuncSet[A] = otherSet

    override def map[B](f: A => B): FuncSet[B] = Empty()

    override def flatMap[B](f: A => FuncSet[B]): FuncSet[B] = Empty()

    override def filter(f: A => Boolean): FuncSet[A] = this

    override def foreach(f: A => Unit): Unit = ()
  }


  case class Cons[A](head: A, tail: FuncSet[A]) extends FuncSet[A] {

    override def contains(elem: A): Boolean =
      elem == head || tail.contains(elem)

    override infix def +(elem: A): FuncSet[A] = Cons(elem, this)

    override infix def ++(otherSet: FuncSet[A]): FuncSet[A] = tail ++ otherSet + head

    override def map[B](f: A => B): FuncSet[B] = tail.map(f) + f(head)

    override def flatMap[B](f: A => FuncSet[B]): FuncSet[B] = tail.flatMap(f) ++ f(head)

    override def filter(f: A => Boolean): FuncSet[A] =
      if f(head) then filter(tail) + head
      else filter(tail)

    override def foreach(f: A => Unit): Unit =
      f(head)
      tail.foreach(f)
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
  }
}
