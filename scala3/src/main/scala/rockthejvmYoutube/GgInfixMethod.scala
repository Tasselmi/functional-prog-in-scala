package rockthejvmYoutube

object GgInfixMethod {

  ////////////////////////////////////////////////////////////////////////////////
  // 1. infix call need annotation / scala.anotation.infix deprecated in 3.1
  ////////////////////////////////////////////////////////////////////////////////
  case class Person(name: String) {
    infix def sings(song: String) = println(s"$name sings $song")
  }

  val p1 = Person("liangfan")
  p1.sings("the day you went away")
  p1 sings "lalala" //if not use infix to decorate methods will emit warn




  ////////////////////////////////////////////////////////////////////////////////
  // 2. use extension key word to extends existing class's functionality
  ////////////////////////////////////////////////////////////////////////////////
  extension (person: Person) {
    infix def enjoys(music: String) = println(s"${person.name} love music $music")
  }

  p1.enjoys("my heart will go on")


  def main(args: Array[String]): Unit = {

  }
}
