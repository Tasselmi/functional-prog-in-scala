package com.rockthejvm.cats.part2abstractMath

object MonadsPractice {

  import cats.Monad
  import cats.syntax.flatMap.*
  import cats.syntax.functor.*
  import cats.instances.list.*
  import cats.instances.either.*

  val aManualEither: Either[String, Int] = Right(42)
  type LoadingOr[T] = Either[String, T]
  type ErrorOr[T]   = Either[Throwable, T]
  val loadingMonad = Monad[LoadingOr]
  val anEither     = loadingMonad.pure(45)

  val aChangedLoading = loadingMonad.flatMap(anEither)(a =>
    if (a % 2 == 0) Right(a + 1) else Left("loading...")
  )

  /**
    * TODO: the service layer API of a web app
    * 
    * 
    * requirements:
    *   - if the host and port are found in the configuration map, then we'll return a M contaning a connection with those values
    *     otherwise the method will fail, according to the logic of the type M
    *   - the issueRequest method returns a M containing the string: "request(payload) has been accepted", if the payload is 
    *     less than 20 characters, otherwise the method will fail, according to the logic of the type M
    * 
    * TODO: provide a real implementation of HttpService using Try, Option, Future, Either
    * 
    */
  case class Connection(host: String, port: String)
  val config = Map("host" -> "localhost", "port" -> "4040")

  trait HttpService[M[_]] {
    def getConnection(cfg: Map[String, String]): M[Connection]
    def issueRequest(connection: Connection, payload: String): M[String]
  }

  def getResponse[M[_]](service: HttpService[M], payload: String)(using
      monad: Monad[M]
  ): M[String] = for {
    conn <- service.getConnection(config)
    resp <- service.issueRequest(conn, payload)
  } yield resp

  def getResponseV2[M[_]: Monad](service: HttpService[M], payload: String): M[String] =
    for {
      conn <- service.getConnection(config)
      resp <- service.issueRequest(conn, payload)
    } yield resp

  object OptionHttpService extends HttpService[Option] {

    override def getConnection(cfg: Map[String, String]): Option[Connection] = for {
      h <- cfg.get("host")
      g <- cfg.get("port")
    } yield Connection(h, g)

    override def issueRequest(connection: Connection, payload: String): Option[String] =
      if payload.length >= 20 then None
      else Some(s"request(${payload}) has been accepted")

  }

  object AggressiveHttpService extends HttpService[ErrorOr] {

    override def getConnection(cfg: Map[String, String]): ErrorOr[Connection] =
      if !(cfg.contains("host") && cfg.contains("port")) then
        Left(
          new java.lang.RuntimeException(
            "connection could not be established: invalid configuration"
          )
        )
      else Right(Connection(cfg("host"), cfg("port")))

    override def issueRequest(
        connection: Connection,
        payload: String
    ): ErrorOr[String] =
      if payload.length() >= 20 then
        Left(new java.lang.RuntimeException("payload is too large"))
      else Right(s"request(${payload}) has been accepted")

  }

  def main(args: Array[String]): Unit = {
    val responseOption = for {
      con <- OptionHttpService.getConnection(config)
      isu <- OptionHttpService.issueRequest(con, "Hello, HTTP service")
    } yield isu

    println(responseOption)
    println(getResponse(OptionHttpService, "hello"))
    println(getResponseV2(OptionHttpService, "helloV2"))

    println(getResponseV2(AggressiveHttpService, "hello, aggressive"))
  }

}
