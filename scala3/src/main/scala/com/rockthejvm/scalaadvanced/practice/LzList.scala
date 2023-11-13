package com.rockthejvm.scalaadvanced.practice

/**
  *  - write a lazily evaluated, potentially infinite linked list
  */
abstract class LzList[A] {

  def isEmpty: Boolean
  def head: A
  def tail: LzList[A]

  //utilities
  def #::(elem: A): LzList[A] // prepending
  infix def ++(another: => LzList[A]): LzList[A] //TODO warning

  //classics
  def foreach(f: A => Unit): Unit
  def map[B](f: A => B): LzList[B]
  def flatMap[B](f: A =>  LzList[B]): LzList[B]
  def filter(f: A => Boolean): LzList[A]
  def withFilter(f: A => Boolean): LzList[A] = filter(f)

  //op
  def take(n: Int): LzList[A]
  def takeAsList(n: Int): List[A] = take(n).toList
  def toList: List[A] = {
    @scala.annotation.tailrec
    def toListAuxiliary(remain: LzList[A], acc: List[A]): List[A] =
      if remain.isEmpty then acc.reverse
      else toListAuxiliary(remain.tail, remain.head :: acc)

    toListAuxiliary(this, List[A]())
  }
}


case class LzEmpty[A]() extends LzList[A] {

  override def isEmpty: Boolean = true

  override def head: A = throw new NoSuchElementException

  override def tail: LzList[A] = throw new NoSuchElementException

  override def #::(elem: A): LzList[A] = new LzCons[A](elem, this)

  override infix def ++(another: => LzList[A]): LzList[A] = another

  override def foreach(f: A => Unit): Unit = ()

  override def map[B](f: A => B): LzList[B] = LzEmpty()

  override def flatMap[B](f: A => LzList[B]): LzList[B] = LzEmpty()

  override def filter(f: A => Boolean): LzList[A] = this

  override def take(n: Int): LzList[A] = {
    if n == 0 then this
    else throw new RuntimeException(s"can't take $n elements from an empty lazy list.")
  }
}


class LzCons[A](hd: => A, tl: => LzList[A]) extends LzList[A] {

  override def isEmpty: Boolean = false

  // use call by need = call by name + lazy val
  override lazy val head: A = hd

  override lazy val tail: LzList[A] = tl

  override def #::(elem: A): LzList[A] =
    new LzCons[A](elem, this)
  
  override infix def ++(another: => LzList[A]): LzList[A] =
    new LzCons[A](hd, tail ++ another)

  override def foreach(f: A => Unit): Unit = {
    @scala.annotation.tailrec
    def foreachAssist(lzList: LzList[A]): Unit =  // foreachAssist接受的是一个List参数
      if lzList.isEmpty then
        ()
      else
        f(lzList.head)
        foreachAssist(lzList.tail)

    foreachAssist(this)
  }

//  override def foreach(f: A => Unit): Unit = {
//    f(this.head)
//    tail.foreach(f)
//  }

  override def map[B](f: A => B): LzList[B] =
    new LzCons[B](f(head), tail.map(f))

  override def flatMap[B](f: A => LzList[B]): LzList[B] =
    f(head) ++ tail.flatMap(f)
  
  override def filter(f: A => Boolean): LzList[A] = {
    if f(head) then new LzCons[A](head, tail.filter(f))
    else tail.filter(f)
  }

  override def take(n: Int): LzList[A] = {
    if n < 0 then LzEmpty()
    else if n == 1 then new LzCons[A](head, LzEmpty())
    else new LzCons[A](head, tail.take(n - 1))
  }
}


object LzList {

  def generate[A](start: A)(generator: A => A): LzList[A] =
    new LzCons[A](start, LzList.generate(generator(start))(generator))

  def from[A](list: List[A]): LzList[A] = list.reverse.foldLeft(LzEmpty[A](): LzList[A]) { (currentLzList, newElement) =>
    new LzCons[A](newElement, currentLzList)
  }

  def apply[A](elements: A*): LzList[A] = from(elements.toList)

  /**
    * Lazy list of Fibonacci numbers
    * @return
    */
  def fibonacci: LzList[BigInt] = {
    def fiboAssist(first: BigInt, second: BigInt): LzList[BigInt] =
      new LzCons[BigInt](first, fiboAssist(second, first + second))

    fiboAssist(1, 2)
  }

  /**
    * infinite list of prime numbers
    *  - filter with isPrime
    *  - Eratosthenes' sieve
    * @return
    */
  def eratosthenes: LzList[Int] = {
    def isPrime(n: Int) = {
      @scala.annotation.tailrec
      def isPrimeTailrec(divisor: Int): Boolean = {
        if (divisor < 2) true
        else if (n % divisor == 0) false
        else isPrimeTailrec(divisor - 1)
      }

      isPrimeTailrec(n / 2)
    }

    // tailrec 需要唯一的递归调用在结尾
    def sieve(numbers: LzList[Int]): LzList[Int] = {
      if (numbers.isEmpty)
        numbers
      else if (!isPrime(numbers.head)) /* 不是素数，递归调用即可 */
        sieve(numbers.tail)
      else /* 是素数，那么要把尾巴中能被该素数整除的先剔除掉 */
        new LzCons[Int](numbers.head, sieve(numbers.tail.filter(_ % numbers.head != 0)))
    }

    val numsFrom2 = LzList.generate(2)(_ + 1)
    sieve(numsFrom2)
  }
}


object LzListPlayground {

  val naturals = LzList.generate(1)(n => n + 1)
  println(naturals.head)
  println(naturals.tail.head)

  val first100 = naturals.take(100)
  first100.foreach(println)

  val first50000 = naturals.take(50000)
  first50000.foreach(println) //java.lang.StackOverflowError if not tailrec

  val first50kList = first50000.toList
  println(first50kList)


  //classics
  println(naturals.map(_ * 2).takeAsList(100))
  println(naturals.flatMap(x => LzList(x, x + 1)).takeAsList(100))

  println(naturals.filter(_ < 10).takeAsList(9))
  //println(naturals.filter(_ < 10).takeAsList(10)) //will infinite search


  //fibonacci
  val fibos = LzList.fibonacci
  println(fibos.takeAsList(30))


  //primes
  val primes = LzList.eratosthenes
  println(primes.takeAsList(30))

  //standard lazy list
  import scala.collection.immutable.LazyList
  val ls = LazyList(1, 2, 3)
  ls.foreach(println)


  def main(args: Array[String]): Unit = {

  }
}
