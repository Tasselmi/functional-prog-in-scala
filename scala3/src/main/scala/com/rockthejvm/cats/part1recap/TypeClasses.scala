package com.rockthejvm.cats.part1recap

object TypeClasses {

  case class Person(name: String, age: Int)

  // part 1 -- type class definition
  trait JsonSerializer[T] {
    def toJson(value: T): String
  }

  // part 2 -- create implicit type class instances
  implicit object StringSerializer extends JsonSerializer[String] {
    override def toJson(value: String): String = "\"" + "\""
  }

  implicit object IntSerializer extends JsonSerializer[Int] {
    override def toJson(value: Int): String = value.toString()
  }

  implicit object PersonSerializer extends JsonSerializer[Person] {
    override def toJson(value: Person): String =
      s"""{"name": ${value.name}, "age": ${value.age}}"""
  }

  // part 3 -- offer some API
  def convertList2Json[T](list: List[T])(using
      serializer: JsonSerializer[T]
  ): String =
    list.map(v => serializer.toJson(v)).mkString("[", ",", "]")

  // part 4 -- extending the existing types via extension methods
  extension [T](value: T) {
    def toJson(using serializer: JsonSerializer[T]): String =
      serializer.toJson(value)
  }

  def main(args: Array[String]): Unit = {
    println("hello, world!")

    val personList = List(Person("liangfan", 35), Person("Alice", 18))
    println(convertList2Json(personList))

    val bob = Person("bob", 22)
    println(bob.toJson)
  }
}
