package practice.datastructure

enum MyList[+A]:
  case Nil
  case Cons(head: A, tail: MyList[A])


object MyList {
  def apply[A](elems: A*): MyList[A] =
    if (elems.isEmpty) then Nil
    else Cons(elems.head, apply(elems.tail*))

  @scala.annotation.tailrec
  def foldLeft[A, B](ml: MyList[A], acc: B, f: (B, A) => B): B = ml match {
    case Nil => acc
    case Cons(h, t) => foldLeft(t, f(acc, h), f)
  }

  def main(args: Array[String]): Unit = {
    def append(x: String, y: Int) = s"x-${y}"

    val ml = MyList(1, 2, 3)
    println(ml.toString)

    val fl = foldLeft(ml, "x", append)
    println(fl)
  }
}
