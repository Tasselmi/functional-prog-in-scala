package com.rockthejvm.cats.part2abstractMath

import cats.Semigroup
import cats.instances.int.*
import cats.instances.string.*

object Semigroups {

  val naturalIntSemigroup = Semigroup[Int]
  val intCombination      = naturalIntSemigroup.combine(3, 5)

  val stringSemigroup   = Semigroup[String]
  val stringCombination = stringSemigroup.combine("I love ", "Cats.")

  def reduceInts(list: List[Int]): Int = list.reduce(naturalIntSemigroup.combine)

  def reduceStrings(list: List[String]): String = list.reduce(stringSemigroup.combine)

  // general API
  def reduceThingsScala2[T](list: List[T])(implicit semigroup: Semigroup[T]): T =
    list.reduce(semigroup.combine)

  def reduceThings[T](list: List[T])(using semigroup: Semigroup[T]): T =
    list.reduce(semigroup.combine)

  /**
    * TODO 1: support a new type
    */
  case class Expense(id: Long, amount: Double)

  given expenseSemigroup: Semigroup[Expense] with {

    override def combine(e1: Expense, e2: Expense): Expense =
      Expense(Math.max(e1.id, e2.id), e1.amount + e2.amount)
  }
  // implicit val expenseSemigroupV2: Semigroup[Expense] =
  //   Semigroup.instance[Expense] { (x, y) =>
  //     Expense(Math.max(x.id, y.id), x.amount + y.amount)
  //   }

  /**
    * extension methods from semigroup
    */
  import cats.syntax.semigroup.*
  val intSum     = 1 |+| 2
  val expenseSum = Expense(1, 12) |+| Expense(3, 1.1)

  def reduceThingsV2[T](list: List[T])(using semigroup: Semigroup[T]): T =
    list.reduce(_ |+| _)

  // [T: Semigroup] means that the compiler will have access to an implicit semigroup of T
  def reduceThingsV3[T: Semigroup](list: List[T]): T = list.reduce(_ |+| _)

  @main
  def run = {
    println("hello world")

    println(intCombination)
    println(stringCombination)

    val numbers = (1 to 10).toList
    val strings = List("I am ", "starting ", "boom")

    println(reduceThings(numbers))
    println(reduceThings(strings))

    import cats.instances.option.*
    val numberOptions = numbers.map(Option(_))
    println(reduceThings(numberOptions))

    val expenseList = List(Expense(1, 10.5), Expense(2, 30))
    println(reduceThings(expenseList))
    println(reduceThingsV3(expenseList))
  }
}
