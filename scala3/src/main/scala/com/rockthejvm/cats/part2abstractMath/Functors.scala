package com.rockthejvm.cats.part2abstractMath

import scala.util.Try

object Functors {

  val aModifiedList = List(1, 2, 3).map(_ + 1)

  // simplified definition
  trait MyFunctor[F[_]] {
    def map[A, B](v: F[A])(f: A => B): F[B]
  }

  import cats.Functor
  import cats.instances.list.*

  val listFunctor     = Functor[List]
  val incrementedList = listFunctor.map(List(1, 2, 3))(_ + 1)

  import cats.instances.try_.*
  val anIncrementedTry = Functor[Try].map(Try(42))(_ + 1)

  def do10x[F[_]](container: F[Int])(using functor: Functor[F]): F[Int] =
    functor.map(container)(_ * 10)

  /**
    * TODO 1: define your own functor for a binary tree
    */
  trait Tree[+T]

  object Tree {
    // smart constructors
    def leaf[T](v: T): Tree[T]                                  = Leaf(v)
    def branch[T](v: T, left: Tree[T], right: Tree[T]): Tree[T] = Branch(v, left, right)
  }

  case class Leaf[+T](value: T)                                  extends Tree[T]
  case class Branch[+T](value: T, left: Tree[T], right: Tree[T]) extends Tree[T]

  given treeFunctor: Functor[Tree] with {

    override def map[A, B](fa: Tree[A])(f: A => B): Tree[B] = fa match
      case Leaf(v)           => Leaf(f(v))
      case Branch(v, le, ri) => Branch(f(v), map(le)(f), map(ri)(f))

  }

  /**
    * extension methods - add map method to the functor
    */
  import cats.syntax.functor.*
  val tree: Tree[Int] = Tree.branch(30, Tree.leaf(10), Tree.leaf(20))
  val incrementedTree = tree.map(_ + 1)

  /**
    * TODO 2: write a shoted do10x method using extension methods
    * 
    * 黑魔法之： implicit + using + type bounds
    * 
    */
  def do10xShorted[F[_]](container: F[Int])(using functor: Functor[F]): F[Int] =
    container.map(_ * 10)

  def do10xShortedV2[F[_]: Functor](container: F[Int]): F[Int] = container.map(_ * 10)

  // do10xShortedV2 和下面的 reduceThingsV3 有类似用法
  // [T: Semigroup] means that the compiler will have access to an implicit semigroup of T
  import cats.Semigroup
  import cats.syntax.semigroup.*

  def reduceThingsV3[T: Semigroup](list: List[T]): T = list.reduce(_ |+| _)

  def main(args: Array[String]): Unit = {
    println(do10x(List(1, 2, 3)))
    println(do10x(Option(1)))

    println(do10x(Tree.branch(30, Tree.leaf(10), Tree.leaf(20))))

    println(do10xShorted(List(1, 2, 3)))
    println(do10xShortedV2(List(1, 2, 3)))
  }

}
