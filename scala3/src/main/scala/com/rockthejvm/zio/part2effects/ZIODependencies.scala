package com.rockthejvm.zio.part2effects

import zio.*

import java.io.IOException
import java.util.concurrent.TimeUnit

object ZIODependencies extends ZIOAppDefault {

  private val userList: List[User] = List(
    User("liangfan", "fan.liang@shopee.com"),
    User("nj", "juan.ni@shopee.com"),
    User("zhangsan", "san.zhang@shopee.com")
  )

  case class User(name: String, email: String)

  /*  database */
  case class Connection() {
    def runQuery(query: String): Task[Unit] = ZIO.succeedUnsafe(_ => println(s"executing query: $query"))
  }

  class ConnectionPool(nConnections: Int) {
    def get: Task[Connection] =
      ZIO.succeedUnsafe(_ => println("acquired connection")) *> ZIO.succeedUnsafe(_ => Connection())
  }

  object ConnectionPool {
    def create(n: Int): ConnectionPool = new ConnectionPool(n)

    def live(n: Int): ZLayer[Any, Nothing, ConnectionPool] = ZLayer.succeed(create(n))
  }

  class UserDatabase(conPool: ConnectionPool) {
    def insert(user: User): Task[Unit] = for {
      conn <- conPool.get
      _ <- conn.runQuery(s"insert into subscribers(name, email) values ('${user.name}', '${user.email}')")
    } yield ()
  }

  object UserDatabase {
    def create(connectionPool: ConnectionPool): UserDatabase = new UserDatabase(connectionPool)

    val live: ZLayer[ConnectionPool, Nothing, UserDatabase] = ZLayer.fromFunction(create)
  }

  /* email service */
  class EmailService {
    def email(user: User): Task[Unit] = ZIO.succeedUnsafe { _ =>
      println(s"you've just been subscribed to Rock the JVM. Welcome, ${user.name} !")
    }
  }

  object EmailService {
    def create(): EmailService = new EmailService()

    val live: ZLayer[Any, Nothing, EmailService] = ZLayer.succeed(create())
  }

  /* user subscription */
  class UserSubscription(emailService: EmailService, userDatabase: UserDatabase) {
    def subscribeUser(user: User): Task[Unit] = for {
      _ <- emailService.email(user)
      _ <- userDatabase.insert(user)
    } yield ()
  }

  object UserSubscription {
    def create(emailService: EmailService, userDatabase: UserDatabase): UserSubscription =
      new UserSubscription(emailService, userDatabase)

    val live: ZLayer[EmailService & UserDatabase, Nothing, UserSubscription] = ZLayer.fromFunction(create)
  }

  /**
    *
    * '''Dependency Injection'''
    *
    * "clean DI" has drawbacks
    *  - does not scale for many services [如果服务数量一多，那么很难扩展很难debug，简直就是一场噩梦，所以说扩展性差]
    *  - DI can be 100x worse
    *   1. pass dependencies partially
    *   1. not having all dependencies in the same place
    *   1. passing dependencies multiple times [可能引发程序错误、资源泄露等]
    *
    */
  val subscriptionService: UIO[UserSubscription] = ZIO.succeedUnsafe { _ =>
    UserSubscription.create(
      EmailService.create(),
      UserDatabase.create(ConnectionPool.create(10))
    )
  }

  def subscribe(user: User): ZIO[Any, Throwable, Unit] = for {
    sub <- subscriptionService  //service is instantiated at the point of call
    _ <- sub.subscribeUser(user)
  } yield ()

  //risk leaking resouces if you subscribe multiple users in the same program
  val program = for {
    _ <- subscribe(userList.head)
    _ <- subscribe(userList.tail.head)
  } yield ()


  /**
    * '''alternative choice'''
    *
    *  - we don not need to care about dependencies until the end of the world
    *  - all ZIOs requiring this dependency will use the same instance [在provide那一刻才会实例化一次传进去]
    *  - can use different instances of the same type for different needs (e.g. testing)
    */
  def subscribeV2(user: User): ZIO[UserSubscription, Throwable, Unit] = for {
    sub <- ZIO.service[UserSubscription]
    _ <- sub.subscribeUser(user)
  } yield ()

  val programV2 = for {
    _ <- subscribeV2(userList.head)
    _ <- subscribeV2(userList.tail.head)
  } yield ()


  /**
    * '''ZLayers'''
    *
    */
  val connectionPoolLayer: ULayer[ConnectionPool] = ZLayer.succeed(ConnectionPool.create(10))
  val databaseLayer: ZLayer[ConnectionPool, Nothing, UserDatabase] = ZLayer.fromFunction(UserDatabase.create)
  val emailServiceLayer: ULayer[EmailService] = ZLayer.succeed(EmailService.create())
  val userSubscriptionServiceLayer: ZLayer[UserDatabase & EmailService, Nothing, UserSubscription] =
    ZLayer.fromFunction(UserSubscription.create)

  // composing layers
  val databaseLayerFull: ZLayer[Any, Nothing, UserDatabase] = connectionPoolLayer >>> databaseLayer
  val subscriptionRequirementsLayer: ZLayer[Any, Nothing, UserDatabase & EmailService] =
    databaseLayerFull ++ emailServiceLayer
  val userSubscriptionLayer = subscriptionRequirementsLayer >>> userSubscriptionServiceLayer

  // best practice: write factory methods to expose layers in the companion objects of the services
  val runnableProgram =
    //subscribe(userList.head)
    //program
    //programV2.provideLayer(ZLayer.fromZIO(subscriptionService))
    programV2.provideLayer(userSubscriptionLayer)

  val runnableProgramV2 = programV2.provide(
    ZLayer.Debug.mermaid,
    EmailService.live,
    ConnectionPool.live(10),
    UserDatabase.live,
    UserSubscription.live,
    ZLayer.Debug.mermaid
  )

  /**
    * '''provided services'''
    *
    */
  val userSubscriptionLayerV2 = ZLayer.make[UserSubscription](
    EmailService.live,
    ConnectionPool.live(10),
    UserDatabase.live,
    UserSubscription.live
  )

  //passthrough
  val dbWithPoolLayer: ZLayer[ConnectionPool, Nothing, ConnectionPool & UserDatabase] = UserDatabase.live.passthrough
  //service == take a dependency and expose it as a value to further layers
  val dbService: ZLayer[UserDatabase, Nothing, UserDatabase] = ZLayer.service[UserDatabase]
  //launch == create a ZIO that uses the services and never finish
  val subscriptionLaunch: ZIO[EmailService & UserDatabase, Nothing, Nothing] = UserSubscription.live.launch


  val getTime = Clock.currentTime(TimeUnit.SECONDS)
  val randomValue = Random.nextInt
  val sysVariable = System.env("JAVA_HOME")
  val printline = Console.printLine("Rock the JVM...")


  override def run: ZIO[Any, Any, Any] = runnableProgramV2
}
