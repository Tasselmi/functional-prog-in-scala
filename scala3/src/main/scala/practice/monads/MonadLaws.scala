//package practice
//package monads
//
////import javax.xml.crypto.dsig.Transform
//
//import 
//
//object MonadLaws {
//
//  case class Id[A](id: A) {
//    def map[B](f: A => B): Id[B] = Id[B](f(id))
//
//    def flatMap[B](f: A => Id[B]): Id[B] = f(id)
//  }
//
//  val id1 = Id(1).flatMap(a => Id(", hello").flatMap(b => Id(s"$a" + b)))
//
//  val id2 = for {
//    a <- Id("hello, ")
//    b <- Id("world")
//  } yield a + b
//
//
//  case class State[S, A](run: S => (A, S)) {
//    def map[B](f: A => B): State[S, B] =
//      State((s: S) => {
//        val res = run(s)
//        (f(res._1), res._2)
//      })
//
//    def flatMap[B](f: A => State[S, B]): State[S, B] =
//      State((s: S) => {
//        val res = run(s)
//        f(res._1).run(res._2)
//      })
//  }
//  
//  type IntState[A] = State[Int, A]
//  
//  object IntStateMonad extends Monad[IntState] {
//    override def unit[A](a: => A): IntState[A] = State(s => (a, s))
//    
//    def flatMap[A, B](st: IntState[A])(transform: A => IntState[B]): IntState[B] = st.flatMap(transform)
//  }
//  
//  
//  ////// use type lambdas to generalize any type state
//  given stateMonadViaTypeLambdas[S]: Monad[[T] =>> State[S, T]] with {
//    override def unit[A](a: => A): State[S, A] = State(s => (a, s))
//    //def flatMap[A, B](st: State[S, A])(f: A => State[S, B]) = st.flatMap(f)
//    
//    extension[A] (sa: State[S, A]) {
//      override def flatMap[B](transform: A => State[S, B]) = sa.flatMap(transform)
//    }
//  }
//  
//  val intStateM = stateMonadViaTypeLambdas[Int]
//  val stringStateM = stateMonadViaTypeLambdas[String]
//  
//
//
//  def main(args: Array[String]): Unit = {
//    val ls = List(1, 2, 3)
//
//    //// identity laws
//    println(ls.flatMap(x => List(x)))
//    println(ls.flatMap(x => Some(s"$x-$x")))
//    println(ls.flatMap(x => None))
//    println(ls.flatMap(x => Array(10, 20)))
//    //    List(1, 2, 3)
//    //    List(1, 2, 3)
//    //    List()
//    //    List(10, 20, 10, 20, 10, 20)
//
//
//    println(id1)
//    println(id2)
//  }
//}
