package com.rockthejvm.scalaadvanced.part4context

object ExtensionMethods {

  ////////////////////////////////////////////////////////////////////////////////
  //  1. extension of existing data type
  ////////////////////////////////////////////////////////////////////////////////
  case class Person(name: String) {
    def greet = println(s"Hello, everyone! I am $name.")
  }

  extension (richStr: String) {
    def greetByPersion = Person(richStr).greet

    def absLength = richStr.trim.length
  }

  "liangfan".greetByPersion

  println(" abc ".absLength)

  ////////////////////////////////////////////////////////////////////////////////
  //  2. extension of generic types
  ////////////////////////////////////////////////////////////////////////////////
  extension[A](list: List[A]) {
    def ends = (list.head, list.tail)
  }

  val aList = List(1, 2, 3, 4)
  println(aList.ends)
  println(ends(aList))


  ////////////////////////////////////////////////////////////////////////////////
  //  3. make APIs expressive
  ////////////////////////////////////////////////////////////////////////////////
  trait Combinator[A] {   /* === trait Semigroup */
    def combine(x: A, y: A): A
  }

  extension[A](list: List[A]) {
    def combineAll(using combinator: Combinator[A]): A = list.reduce(combinator.combine)
  }

  given intSumCombinator: Combinator[Int] with {
    override def combine(x: Int, y: Int): Int = x + y
  }

  given stringCombinator: Combinator[String] with {
    override def combine(x: String, y: String): String = x + " " + y
  }

  println(aList.combineAll)
  println(List("abc", "efg", "xyz").combineAll)


  ////////////////////////////////////////////////////////////////////////////////
  //  4. exercises: enhance existing types after they've been defined
  ////////////////////////////////////////////////////////////////////////////////

  /**
    *  - add an isPrime method to the Int type
    *  - add extensions to Tree
    */
  extension (number: Int) {
    def isPrime: Boolean = {
      @scala.annotation.tailrec
      def isPrimeAux(divisor: Int): Boolean = {
        //println(s"divisor is: $divisor")
        if (divisor > number / 2) true
        else if (number % divisor == 0) false
        else isPrimeAux(divisor + 1)
      }

      require(number >= 0)
      if (number <= 1) false
      else isPrimeAux(2)
    }
  }

  sealed abstract class Tree[A]
  case class Leaf[A](value: A) extends Tree[A]
  case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

  extension [A](tree: Tree[A]) {
    infix def map[B](f: A => B): Tree[B] = tree match {
      case Leaf(v) => Leaf(f(v))
      case Branch(lt, rt) => Branch(lt map f, rt map f)
    }

    infix def forall(predicate: A => Boolean): Boolean = tree match {
      case Leaf(v) => predicate(v)
      case Branch(lt, rt) => (lt forall predicate) && (rt forall predicate)
    }

    def combineAll(using combinator: Combinator[A]): A = tree match {
      case Leaf(v) => v
      case Branch(lt, rt) => combinator.combine(lt.combineAll, rt.combineAll)
    }
  }

  extension (tree: Tree[Int]) {
    def sum: Int = tree match {
      case Leaf(v) => v
      case Branch(lt, rt) => lt.sum + rt.sum
    }
  }

  val aTree: Tree[Int] = Branch(Leaf(3), Branch(Leaf(1), Leaf(10)))


  def main(args: Array[String]): Unit = {
//    println(1.isPrime)
//    println(2.isPrime)
//    println(3.isPrime)
//    println(4.isPrime)
//    println(5.isPrime)
//    println(101.isPrime)

    println(aTree.combineAll)
    println(aTree.sum)
    println(aTree.map(_ * 2))
    println(aTree.forall(_ % 2 == 0))
  }
}
