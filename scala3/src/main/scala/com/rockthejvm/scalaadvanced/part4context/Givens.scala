package com.rockthejvm.scalaadvanced.part4context

/**
  * exercises:
  *  - create a given for Option[A] if you can order A
  *  - use summon method to call a value of your given type
  */
object Givens {
  /**
    * The result sign of compare method has the following meaning:
    *  - negative if x < y
    *  - positive if x > y
    *  - zero otherwise (if x == y)
    */
  given optionOrdering[A](using normalOrdering: Ordering[A]): Ordering[Option[A]] with {
    override def compare(x: Option[A], y: Option[A]): Int = (x, y) match {
      case (None, None) => 0
      case (None, _) => 1
      case (_, None) => -1
      case (Some(a), Some(b)) => normalOrdering.compare(a, b) * (-1)
    }
  }

  /**
    * Summon a given value of type `T`.
    * transparent inline def summon[T](using inline x: T): x.type = x
    */
  given optionOrderingV2[A : Ordering]: Ordering[Option[A]] with {
    override def compare(x: Option[A], y: Option[A]): Int = (x, y) match {
      case (None, None) => 0
      case (None, _) => -1
      case (_, None) => 1
      //summon a instance of Ordering[A]
      case (Some(a), Some(b)) => summon.compare(a, b)
    }
  }

  val optionList = List(None, Option(3), Option(-1), Option(7), None)


  def main(args: Array[String]): Unit = {
    println(optionList.sorted(optionOrdering))
    println(optionList.sorted(optionOrderingV2))
  }
}
