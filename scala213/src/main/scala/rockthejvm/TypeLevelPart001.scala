package rockthejvm

import scala.reflect.runtime.universe._

object TypeLevelPart001 {

  //boilerplate  样板文件
  def show[T](value: T)(implicit tag: TypeTag[T]): String = tag.toString()
  println(show(List(1, 2, 3)))


  //type-level programming
  trait Number
  class _0 extends Number
  class Next[N <: Number] extends Number

  type _1 = Next[_0]
  type _2 = Next[_1]
  type _3 = Next[_2]
  type _4 = Next[_3]
  type _5 = Next[_4]

  def main(args: Array[String]): Unit = {
    println("hello world")
  }
}
