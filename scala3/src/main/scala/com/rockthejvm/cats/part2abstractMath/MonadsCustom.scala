package com.rockthejvm.cats.part2abstractMath

import cats.Monad
import com.rockthejvm.interview.recap.ScalaRecap.x
// import cats.syntax.flatMap.*
// import cats.syntax.functor.*

object MonadsCustom {

  implicit object OptionMonad extends Monad[Option] {
    override def pure[A](x: A): Option[A] = Option(x)

    override def flatMap[A, B](fa: Option[A])(f: A => Option[B]): Option[B] =
      fa.flatMap(f)

    @scala.annotation.tailrec
    override def tailRecM[A, B](a: A)(f: A => Option[Either[A, B]]): Option[B] =
      f(a) match
        case None           => None
        case Some(Right(b)) => Some(b)
        case Some(Left(x))  => tailRecM(x)(f)

  }

  /**
    * TODO 1: define a monad for the identity type
    */
  // 又是啥黑魔法，fuck
  type Identity[T] = T
  val aNumber: Identity[Int] = 42

  implicit object IdentidyMonad extends Monad[Identity] {

    // Identity[A] === A
    override def flatMap[A, B](fa: Identity[A])(f: A => Identity[B]): Identity[B] =
      f(fa)

    @scala.annotation.tailrec
    override def tailRecM[A, B](a: A)(f: A => Identity[Either[A, B]]): Identity[B] =
      f(a) match
        case Left(lv)  => tailRecM(lv)(f)
        case Right(rv) => rv

    override def pure[A](x: A): Identity[A] = x

  }

  /**
    * TODO 2
    * harder example: define a monad for a Tree
    */
  sealed trait Tree[+T]

  object Tree {
    // smart constructors
    def leaf[T](v: T): Tree[T]                            = Leaf(v)
    def branch[T](left: Tree[T], right: Tree[T]): Tree[T] = Branch(left, right)
  }

  case class Leaf[+T](value: T)                        extends Tree[T]
  case class Branch[+T](left: Tree[T], right: Tree[T]) extends Tree[T]

  given TreeMonad: Monad[Tree] with {

    override def flatMap[A, B](fa: Tree[A])(f: A => Tree[B]): Tree[B] = fa match
      case Leaf(value)         => f(value)
      case Branch(left, right) => Branch(flatMap(left)(f), flatMap(right)(f))

    override def tailRecM[A, B](a: A)(f: A => Tree[Either[A, B]]): Tree[B] = {
      def stackRec(t: Tree[Either[A, B]]): Tree[B] = t match
        case Leaf(Left(v))       => stackRec(f(v))
        case Leaf(Right(b))      => Leaf(b)
        case Branch(left, right) => Branch(stackRec(left), stackRec(right))
      
      def tailRec(x: Int): Int = ???

      stackRec(f(a))
    }

    override def pure[A](x: A): Tree[A] = Tree.leaf(x)

  }

  def main(args: Array[String]): Unit = {
    println("world")
    println("hello")

    val multiString = """|
      | line 1
      | line 2
      |""".stripMargin

    println(multiString)
  }

}
