package com.rockthejvm.cats.part2abstractMath

import cats.Monoid
import cats.instances.string.*
import cats.instances.int.*
import cats.instances.option.*
import cats.syntax.monoid.*

object Monoids {

  val intMonoid  = Monoid[Int]
  val combineInt = intMonoid.combine(23, 99)
  val zero       = intMonoid.empty

  def combineFold[@specialized(Short, Int, Long, Double, Float) T](list: List[T])(using
      monoid: Monoid[T]
  ): T = list.foldLeft(monoid.empty)(_ |+| _)

  val phoneBooks = List(
    Map("Alice"  -> 225, "Bob" -> 647),
    Map("Daniel" -> 372),
    Map("Tina"   -> 123)
  )

  import cats.instances.map.*

  val massivePhoneBook = combineFold(phoneBooks)

  /**
  * TODO 3: shopping cart and online stores with Monoids
  */
  case class ShoppingCart(items: List[String], total: Double)

  given shoppingCartMonoid: Monoid[ShoppingCart] with {

    override def combine(x: ShoppingCart, y: ShoppingCart): ShoppingCart = ShoppingCart(
      x.items ++ y.items,
      x.total + y.total
    )

    override def empty: ShoppingCart = ShoppingCart(List.empty[String], 0.0)
  }

  def checkout(shoppingCarts: List[ShoppingCart]): ShoppingCart =
    combineFold(shoppingCarts)

  def main(args: Array[String]): Unit = {
    println("hello world")
    println(combineInt)

    println(massivePhoneBook)
  }
}
