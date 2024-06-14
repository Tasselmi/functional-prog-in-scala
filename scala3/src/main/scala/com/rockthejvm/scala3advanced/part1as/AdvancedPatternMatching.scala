package com.rockthejvm.`scala3advanced`.part1as

object AdvancedPatternMatching {

  ////////////////////////////////////////////////////////////////////////////////
  //  1. construct type matching by your own
  ////////////////////////////////////////////////////////////////////////////////
  class Person(val name: String, val age: Int)

  object Person {
    def unapply(p: Person): Option[(String, Int)] = /* person match { case Person(string, int) => } */
      if (p.age < 21) None
      else Some(p.name, p.age)

    def unapply(age: Int): Option[String] = /* int match { case Person(string) => } */
      if (age < 21) Some("minor")
      else Some("legally allowed to drink")
  }

  val fan = new Person("liangfan", 35)
  val fanMatch = fan match {
    case Person(n, a) => s"Hi there, I'm ${n}"
  }
  val fanLegalStatus = fan.age match {
    case Person(status) => s"fan's legal drinking status is ${status}"
  }

  println(fanMatch)
  println(fanLegalStatus)


  ////////////////////////////////////////////////////////////////////////////////
  //  2. boolean patterns
  ////////////////////////////////////////////////////////////////////////////////
  object Even {
    def unapply(arg: Int): Boolean = arg % 2 == 0
  }

  object SingleDigit {
    def unapply(arg: Int): Boolean = arg > -10 && arg < 10
  }

  val n = 7
  val mathProp = n match {
    case Even() => "an even number"
    case SingleDigit() => "a one digit number"
    case _ => "no special property"
  }

  println(mathProp)


  ////////////////////////////////////////////////////////////////////////////////
  //  3. infix patterns
  ////////////////////////////////////////////////////////////////////////////////
  infix case class Or[A, B](a: A, b: B)

  val anEither = Or(2, "two")
  val humanDescEither = anEither match {
    case num Or str => s"$num is written as $str"
  }

  println(humanDescEither)

  val aList = List(1, 2, 3)
  val listMatch = aList match {
    case 1 :: rest => "a lsit starts with one"
    case _ => "some other list"
  }

  println(listMatch)

  ////////////////////////////////////////////////////////////////////////////////
  //  4. decomposing sequences
  ////////////////////////////////////////////////////////////////////////////////
  val vararg = aList match {
    case List(1, _*) => "list starting with 1"
    case _ => "some other list"
  }

  abstract class MyList[A] {
    def head: A = throw new NoSuchElementException
    def tail: MyList[A] = throw new NoSuchElementException
  }
  case class Empty[A]() extends MyList[A]
  case class Cons[A](override val head: A, override val tail: MyList[A]) extends MyList[A]

  object MyList {
    def unapplySeq[A](list: MyList[A]): Option[Seq[A]] =
      if (list == Empty())
        Some(Seq.empty)
      else
        unapplySeq(list.tail).map(rest => list.head +: rest)
  }

  val myList: MyList[Int] = Cons(1, Cons(2, Cons(3, Empty())))
  val varargCustom = myList match {
    case MyList(1, _*) => "my list starts with 1"
    case _ => "some other list"
  }

  println(varargCustom)


  ////////////////////////////////////////////////////////////////////////////////
  //  5. custom return type for unapply
  ////////////////////////////////////////////////////////////////////////////////
  abstract class Wrapper[T] {
    def isEmpty: Boolean
    def get: T
  }

  object PersonWrapper {
    def unapply(p: Person): Wrapper[String] = new Wrapper[String] {
      override def isEmpty: Boolean = false
      override def get: String = p.name
    }
  }

  val weirdPersonMatch = fan match {
    case PersonWrapper(n) => s"Hey, my name is $n"
  }

  println(weirdPersonMatch)


  def main(args: Array[String]): Unit = {

  }
}
