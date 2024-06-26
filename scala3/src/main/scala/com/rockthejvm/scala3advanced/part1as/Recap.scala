package com.rockthejvm.`scala3advanced`.part1as

object Recap {
  
  ////////////////////////////////////////////////////////////////////////////////
  // 1. values, types, expressions
  ////////////////////////////////////////////////////////////////////////////////
  val aCondition = false //vals are constants
  val anIfExpression = if (aCondition) 42 else 55 //expressions evaluate to a value

  val aCodeBlock = {
    if (aCondition) then
      54
    else
      0
    11
  }

  println(aCodeBlock)


  ////////////////////////////////////////////////////////////////////////////////
  //   2. types: Int, String, Double, Boolean, Char...
  //      Unit = () == "void" in other languages
  ////////////////////////////////////////////////////////////////////////////////
  val theUnit = println("Hello, Scala")


  ////////////////////////////////////////////////////////////////////////////////
  //  3. functions
  ////////////////////////////////////////////////////////////////////////////////
  def aFunction(x: Int): Int = x + 1


  ////////////////////////////////////////////////////////////////////////////////
  //  4. recursion: stack & tail
  ////////////////////////////////////////////////////////////////////////////////
  @scala.annotation.tailrec
  def factorial(n: Int, acc: Int): Int =
    if (n <= 0) acc
    else factorial(n - 1, n * acc)

  val fact10 = factorial(10, 1)

  println(fact10)


  ////////////////////////////////////////////////////////////////////////////////
  //  5. objec oriented
  ////////////////////////////////////////////////////////////////////////////////
  class Animal
  class Dog extends Animal
  val aDog: Animal = new Dog

  trait Carnivore { /* 食肉动物 */
    infix def eat(a: Animal): Unit
  }

  class Crocodile extends Animal with Carnivore {
    override infix def eat(a: Animal): Unit = println("I am a crocodile, I eat everything")
  }

  ////////////////////////////////////////////////////////////////////////////////
  //  6. method notation
  ////////////////////////////////////////////////////////////////////////////////
  val aCroc = new Crocodile
  aCroc.eat(aDog)
  aCroc eat aDog


  ////////////////////////////////////////////////////////////////////////////////
  //  7. anonymous classes
  ////////////////////////////////////////////////////////////////////////////////
  val aCarnivore = new Carnivore {
    override infix def eat(a: Animal): Unit = println("I am a carnivore")
  }


  ////////////////////////////////////////////////////////////////////////////////
  //  8. generics
  ////////////////////////////////////////////////////////////////////////////////
  abstract class LinkList[A] {
    //type A is known inside the implementation
  }



  ////////////////////////////////////////////////////////////////////////////////
  //  10. singletons and companions
  ////////////////////////////////////////////////////////////////////////////////
  object LinkList /* used for instance-independent fields/methods --like "static" in java */


  ////////////////////////////////////////////////////////////////////////////////
  //  11. case classes
  ////////////////////////////////////////////////////////////////////////////////
  case class Person(name: String, age: Int) /* apply method, hashcode, serializable */


  ////////////////////////////////////////////////////////////////////////////////
  //  12. enums
  ////////////////////////////////////////////////////////////////////////////////
  enum BasicColors {
    case RED, GREEN, BLUE
  }


  ////////////////////////////////////////////////////////////////////////////////
  //  13. exceptions and try/catch/finally
  ////////////////////////////////////////////////////////////////////////////////
  def throwSomeException(): Int =
    throw new RuntimeException

  val aPotentialFailure = try {
    throwSomeException()
  } catch {
    case e: Exception => "I caught an exception"
  } finally {
    println("finally close resource")
  }


  ////////////////////////////////////////////////////////////////////////////////
  //  14. functional programming
  ////////////////////////////////////////////////////////////////////////////////
  val incrementer = new Function[Int, Int] {
    override def apply(v1: Int): Int = v1 + 1
  }

  val two = incrementer(1)


  ////////////////////////////////////////////////////////////////////////////////
  //  15. lambdas
  ////////////////////////////////////////////////////////////////////////////////
  val anonymousIncrementer = (x: Int) => x + 1


  ////////////////////////////////////////////////////////////////////////////////
  //  16. high-order functions
  ////////////////////////////////////////////////////////////////////////////////
  val anIncrementerList = List(1, 2, 3).map(anonymousIncrementer)
  println(anIncrementerList.mkString("[ ", ", ", " ]"))


  ////////////////////////////////////////////////////////////////////////////////
  //  17. for-comprehensions
  ////////////////////////////////////////////////////////////////////////////////
  val pair1 = for {
    number <- List(1, 2, 3)
    str <- List("a", "b", "c")
  } yield s"$str-$number"

  val pair2 = List(1, 2, 3).flatMap { number =>
    List("a", "b", "c").flatMap { str =>
      List(s"$str-$number")
    }
  }

  println(pair1)
  println(pair2)


  ////////////////////////////////////////////////////////////////////////////////
  //  18. scala collections: Seq, Array, List, Vector, Map, Tuple, Set
  ////////////////////////////////////////////////////////////////////////////////
  

  ////////////////////////////////////////////////////////////////////////////////
  //  19. Option, Try
  ////////////////////////////////////////////////////////////////////////////////
  val anOption: Option[Int] = Option(42)


  ////////////////////////////////////////////////////////////////////////////////
  //  20. pattern matching
  ////////////////////////////////////////////////////////////////////////////////
  val x = 2
  val order = x match {
    case 1 => "first"
    case 2 => "second"
    case _ => "not important"
  }
  
  val bob = Person("Bob", 22)
  val greeting = bob match {
    case Person(p, _) => s"Hi, my name is $p"
  }


  ////////////////////////////////////////////////////////////////////////////////
  //  21. breceless syntax
  ////////////////////////////////////////////////////////////////////////////////
  val pair3 =   /* same for if, for, while */
    for
      number <- List(1, 2, 3)
      str <- List("a", "b", "c")
    yield s"$str-$number"  


  ////////////////////////////////////////////////////////////////////////////////
  //  22. indentation tokens
  ////////////////////////////////////////////////////////////////////////////////
  class BracelessAnimal:
    def eat: Unit =
      println("I am eating")
      println("always eating")
    end eat
  end BracelessAnimal


  def main(args: Array[String]): Unit = {

  }
}
