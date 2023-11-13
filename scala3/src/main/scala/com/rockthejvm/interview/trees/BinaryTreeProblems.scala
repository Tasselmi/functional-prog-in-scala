package com.rockthejvm.interview.trees

sealed abstract class BTree[+T] {
  def value: T // or pure function: Option[T]

  def left: BTree[T]

  def right: BTree[T]

  def isEmpty: Boolean
}

case object BinEnd extends BTree[Nothing] {
  override def value: Nothing = throw new NoSuchElementException()

  override def left: BTree[Nothing] = throw new NoSuchElementException()

  override def right: BTree[Nothing] = throw new NoSuchElementException()

  override def isEmpty: Boolean = true
}

case class BinNode[+T](
    override val value: T,
    override val left: BTree[T],
    override val right: BTree[T])
  extends BTree[T] {

  override def isEmpty: Boolean = ???
}

object BinaryTreeProblems extends App {

}
