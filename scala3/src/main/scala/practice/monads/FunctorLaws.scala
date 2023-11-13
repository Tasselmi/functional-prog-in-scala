package practice.monads

object FunctorLaws {


  trait Functor[F[_]] {
    extension[A] (fa: F[A]) {
      def map[B](f: A => B): F[B]
    }
  }

  object Functor {
    given listFunctor: Functor[List] with
      extension[A] (as: List[A]) {
        def map[B](f: A => B): List[B] = as map f
      }
  }


}
