package rockthejvmYoutube

object EeTypeLambdas {


  ////////////////////////////////////////////////////////////////////////////////
  // 1. type system of scala ---- kind = type of types
  ////////////////////////////////////////////////////////////////////////////////

  /*
    first kind -- level-0 types, for example: String, Int, Person...
  */
  val aNumber: Int = 42
  case class Person(name: String, age: Int)
  val p = Person("lf", 20)

  /*
    second kind -- level-1 types, or "generics", for example List, Array
    level-1 type must construct with a level-0 type, can not be used alone
  */
  class LinkedList[T] {

  }
  //val l: LinkedList = ??? //not compile
  val lt: LinkedList[Int] = ???   // lt is level-0 type

  /*
    third kind -- level-2 types, for example Functor, Monad
    level-2 type must construct with a level-1 type, can not be used alone
  */
  class Functor[F[_]]
  val functorList = new Functor[List] //functorList is level 0 type

  /*
    even higher kinds
  */
  class Meta[F[_[_]]] //level-3 types
  val metaFunctor = new Meta[Functor]

  /*
    examples
  */
  class HashMap[K, V]
  val addressBook = new HashMap[String, Int]

  class ComposedFunctor[F[_], G[_]]
  val aFunc = new ComposedFunctor[List, Option]

  class Formatter[F[_], T]
  val fm = new Formatter[Array, String]



  ////////////////////////////////////////////////////////////////////////////////
  // 2. type lambdas
  ////////////////////////////////////////////////////////////////////////////////
  type MyList = [T] =>> List[T] //MyList === List, List is similar to a function = type constructor


  type MapWithStringKey2[T] = Map[String, T]

  type MapWithStringKey = [T] =>> Map[String ,T]  // same meaning, but more powerful
  val addressBook2: MapWithStringKey[String] = Map("k" -> "must a string")

  type SpecialEither = [T, E] =>> Either[E, Option[T]]
  val specialEither1: SpecialEither[Int, String] = Right(Some(2))   //确实这样写以后变得更直观了，太屌了啊 ！！！！
  val specialEither2: SpecialEither[Int, String] = Left("123")



  ////////////////////////////////////////////////////////////////////////////////
  // 3. why need type lambdas
  ////////////////////////////////////////////////////////////////////////////////
  trait Monad[M[_]] {
    def pure[A](value: A): M[A]
    def flatMap[A, B](fa: M[A])(f: A => M[B]): M[B]
  }

  class EitherMonad[E] extends Monad[[T] =>> Either[E, T]] {
    override def pure[A](value: A): Either[E, A] = ???
    override def flatMap[A, B](fa: Either[E, A])(f: A => Either[E, B]): Either[E, B] = ???
  }


  def main(args: Array[String]): Unit = {

  }
}
