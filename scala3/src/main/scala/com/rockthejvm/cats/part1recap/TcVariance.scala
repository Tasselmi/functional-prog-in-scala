package com.rockthejvm.cats.part1recap

import cats.Eq
import cats.instances.int.* // Eq[Int] TC instance
import cats.instances.option.* // Eq[Option[Int]] TC instance
import cats.syntax.eq.*
import scala.util.{Either, Try}

object TcVariance {

  val aComparison = Option(2) === Option(3)
//   val anInvalidComp = Some(2) === None   // Eq[Some[Int]] not found

  // variance
  class Animal
  class Cat extends Animal

  // covariant
  class Cage[+T]
  val cage: Cage[Animal] = new Cage[Cat]

  // contravariant
  class Vet[-T]
  val vet: Vet[Cat] = new Vet[Animal]

  // rule of thumb: "HAS a T" = covariant, "ACT on T" = contravariant
  // variance affect how TC instances are being fetched

  /** 
    * contravariant TC
    */
  trait SoundMaker[-T]
  implicit object AnimalSoundMaker extends SoundMaker[Animal]
  def makeSound[T](using sMaker: SoundMaker[T]): Unit = println("wow")
  makeSound[Animal]
  // 这里需要一个 SoundMaker[Cat]，编译器找的时候能够找到更具体的子类 SoundMaker[Animal] 所以是可以成立的
  makeSound[Cat] // SoundMaker[Animal] <: SoundMaker[Cat], TC instance for Animal is also applicable to cats
  // rule 1: contravariant TCs can use the superclass instance if nothing is available strictly for that type

  implicit object OptionSoundMaker extends SoundMaker[Option[Int]]
  makeSound[Option[Int]]
  makeSound[Some[Int]]
  makeSound[None.type]

  /** 
    * covariant TC
    */
  trait AnimalShow[+T] {
    def show: String
  }

  implicit object GeneralAnimalShow extends AnimalShow[Animal] {

    override def show: String = "animals everywhere"

  }

  implicit object CatsShow extends AnimalShow[Cat] {

    override def show: String = "so many cats"

  }

  def organizeShow[T](using event: AnimalShow[T]): String = event.show
  // rule 2: covariant TCs will always use the more specific TC instance for that type
  //         but mayconfuse the compiler if the general TC is also present

  // rule 3: you can't have both benefits
  // Cats uses INVARIANT TCs
  Option(2) === Option.empty[Int]

  @main
  def run: Unit = {
    println("hellow world")

    println(organizeShow[Cat])
  }
}
