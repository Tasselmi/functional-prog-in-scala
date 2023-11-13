package practice

object FunctionsVsMethods {

  def main(args: Array[String]): Unit = {
    val v: Int => Int = (x: Int) => x + x

    def f(x: Int): Int = x + x

    val v2: Int => Int = f

    val v3 = new Function[Int, Int] {
      override def apply(v1: Int): Int = v1 + v1
    }


    println(v(2))
    println(f(2))

    println(v)
    println(v2)
    println(v3)
  }
}
