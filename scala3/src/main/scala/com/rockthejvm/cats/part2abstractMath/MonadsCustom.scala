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
      def stackRec(t: Tree[Either[A, B]]): Tree[B] = t match {
        case Leaf(Left(v))       => stackRec(f(v))
        case Leaf(Right(b))      => Leaf(b)
        case Branch(left, right) => Branch(stackRec(left), stackRec(right))
      }

      /**
        * 说白了，就是一句话，把一棵树拆散，处理后，再又拼起来的过程
        *
        * @param remaining  待处理的Tree元素
        * @param expanded   处理过的Branch元素
        * @param processed  处理过的Tree元素
        * @return
        */
      @scala.annotation.tailrec
      def tailRec(
          remaining: List[Tree[Either[A, B]]],
          expanded: Set[Tree[Either[A, B]]],
          processed: List[Tree[B]]
      ): Tree[B] =
        if remaining.isEmpty then processed.head
        else
          remaining.head match {
            case Leaf(Left(lv)) => tailRec(f(lv) :: remaining.tail, expanded, processed)
            case Leaf(Right(rv))            =>
              tailRec(remaining.tail, expanded, Leaf(rv) :: processed)
            case node @ Branch(left, right) =>
              if !expanded.contains(node) then // 把node拆解一层放到remaining中进行处理
                tailRec(right :: left :: remaining, expanded + node, processed)
              else // expanded中包含node，说明该node已经被处理过了，该node的下一级元素已经放入了processed中
                val newLeft   = processed.head
                val newRight  = processed.tail.head
                val newBranch = Branch(newLeft, newRight)
                tailRec(remaining.tail, expanded, newBranch :: processed.drop(2))
          }

      tailRec(List(f(a)), Set(), List())

      /* 处理过程示意 tr == tailrec
            _____1_____
         __2__       __3__
        /     \     /     \
       L1     R2   R3     R4

        tr([1], [], []) =
        tr([3, 2, 1], [1], []) =
        tr([R4, R3, 3, 2, 1], [3, 1], []) =
        tr([R3, 3, 2, 1], [3, 1], [B4]) =
        tr([3, 2, 1], [3, 1], [B3, B4]) =
        tr([2, 1], [1], [B34]) =
        tr([R2, L1, 2, 1], [2, 1], [B34]) =
        tr([L1, 2, 1], [2, 1], [B2, B34]) =
        tr([R1, 2, 1], [2, 1], [B2, B34]) =
        tr([2,1], [2, 1], [B1, B2, B34]) =
        tr([1], [1], [B12, B34]) =
        tr([], [], [B1234]) =
        B1234
       */
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

    val tree        = Branch(Leaf(10), Leaf(20))
    val changedTree = TreeMonad.flatMap(tree)(t => Branch(Leaf(t + 1), Leaf(t + 2)))
    println(changedTree)
  }

}
