package com.rockthejvm.`scala3advanced`.part2afp

object FunctionalCollections {

  ////////////////////////////////////////////////////////////////////////////////
  //  1. sets are functions: A => Boolean
  ////////////////////////////////////////////////////////////////////////////////
  val aSet: Set[String] = Set("I", "love", "Scala")
  val setContainsScala: Boolean = aSet("Scala")


  ////////////////////////////////////////////////////////////////////////////////
  //  2. Seq extends PartialFunction[Int, A]
  ////////////////////////////////////////////////////////////////////////////////
  val aSeq: Seq[Int] = Seq(1, 2, 3, 4)
  val anElem = aSeq(1)  //2
  //val aNonExistElem = aSeq(100) //throw java.lang.IndexOutOfBoundsException


  ////////////////////////////////////////////////////////////////////////////////
  //  3. Map[K, V] extends PartialFunction[K, V]
  ////////////////////////////////////////////////////////////////////////////////
  val aPhoneBook: Map[String, Long] = Map(
    "liang" -> 123456788,
    "fan" -> 123456789
  )

  val fanPhone = aPhoneBook("fan")
  //val alicePhone = aPhoneBook("Alice") //throw java.util.NoSuchElementException


  def main(args: Array[String]): Unit = {

  }
}
