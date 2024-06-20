package com.rockthejvm.cats.part2abstractMath

object MonadTransformers {

  /**
    * option tranformer
    */
  import cats.data.OptionT
  import cats.instances.list.*

  val listOfNumberOptions: OptionT[List, Int] = OptionT(List(Option(1), Option(2)))

  val listOfCharOptions: OptionT[List, Char] =
    OptionT(List(Option('a'), Option('b'), Option.empty[Char]))

  val listOfTuples: OptionT[List, (Int, Char)] = for {
    number <- listOfNumberOptions
    char   <- listOfCharOptions
  } yield (number, char)

  /**
    * either transformer
    */
  import cats.data.EitherT

  val listOfEithers: EitherT[List, String, Int] =
    EitherT(List(Left("something wrong..."), Right(42), Right(66)))

  import scala.concurrent.ExecutionContext.Implicits.global
  import scala.concurrent.Future
  import scala.concurrent.Await
  import scala.concurrent.duration.*

  val futureOfEithers: EitherT[Future, String, Int] = EitherT(Future(Right(22)))

  /**
    * TODO exercise
    * 
    * We have a multi-machine cluster for your business which will receive a traffic surge following a media appearance.
    * We measure bandwidth in units.
    * We want to allocate TWO of our servers to cope with the traffic spike.
    * We know the current capacity for each server and we know we'll hold the traffic if the sum of bandwidths is > 250.
    * 
    */
  val bandwidths = Map(
    "server1.rockthejvm.com" -> 50,
    "server2.rockthejvm.com" -> 300,
    "server3.rockthejvm.com" -> 170
  )

  type AsyncResponse[T] = EitherT[Future, String, T]

  def getBandwidth(server: String): AsyncResponse[Int] = bandwidths.get(server) match
    case None     => EitherT.left(Future(s"server ${server} unreachable..."))
    case Some(rv) => EitherT.right(Future(rv))

  def canWithstandSurge(s1: String, s2: String): AsyncResponse[Boolean] = for {
    band1 <- getBandwidth(s1)
    band2 <- getBandwidth(s2)
  } yield band1 + band2 > 250

  def generateTrafficSpikeReport(s1: String, s2: String): AsyncResponse[String] =
    canWithstandSurge(s1, s2).transform {
      case Left(reason) =>
        Left(s"servers $s1 and $s2 cannot cope with the incoming spike: $reason")
      case Right(false) => Left(
          s"servers $s1 and $s2 cannot cope with the incoming spike, not enough total bandwidth"
        )
      case Right(true)  =>
        Right(s"servers $s1 and $s2 can cope with the incoming spike, No Problem")
    }

  def main(args: Array[String]): Unit = {
    println("hello")
    println("world")

    val resultFuture1 = generateTrafficSpikeReport(
      "server1.rockthejvm.com",
      "server3.rockthejvm.com"
    ).value
    val resultFuture2 = generateTrafficSpikeReport(
      "server1.rockthejvm.com",
      "server2.rockthejvm.com"
    ).value
    println(Await.result(resultFuture1, 2.seconds))
    println(Await.result(resultFuture2, 2.seconds))
  }

}
