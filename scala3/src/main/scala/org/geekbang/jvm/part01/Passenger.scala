package org.geekbang.jvm.part01

abstract class Passenger {

  def passThroughImmigration(): Unit

  override def toString: String = s"override version of Passenger"
}

class ForeignerPassenger extends Passenger {

  override def passThroughImmigration(): Unit = {}
}

class ChinesePassenger extends Passenger {

  override def passThroughImmigration(): Unit = {}

  def visitDutyFreeShops(): Unit = {}
}

/*
  1496
  6112

-XX:CompileCommand=dontinline,*.passThroughImmigration
  3159
  6668

*/
object Main {
  def main(args: Array[String]): Unit = {
    val a = new ChinesePassenger()
    val b = new ForeignerPassenger()
    var current = System.currentTimeMillis()

    (1 to 2000000000) foreach { i =>
      if (i % 1000000000 == 0) then
        val temp = System.currentTimeMillis()
        println(temp - current)
        current = temp

      val c = if (i < 1000000000) then a else b
      c.passThroughImmigration()
    }
  }
}