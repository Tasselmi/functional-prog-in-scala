package com.rockthejvm.zio.part3concurrency

import zio.*
import com.rockthejvm.zio.utils.*
import java.util.concurrent.{ExecutorService, Executors}
import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success, Try}


object AsynchronousEffects extends ZIOAppDefault {

  //callback-based
  object LoginService {
    case class AuthError(message: String)
    case class UserProfile(email: String, name: String)

    //thread pool
    val executor = Executors.newFixedThreadPool(4)

    //"database"
    val passwd = Map("fan.liang@shopee.com" -> "RockTheJVM1")

    //the profile data
    val database = Map("fan.liang@shopee.com" -> "liangfan")

    def login(email: String, password: String)(onSuccess: UserProfile => Unit, onFailure: AuthError => Unit): Unit =
      executor.execute { () =>
        println(s"[${Thread.currentThread().getName}] attempting login for $email")

        passwd.get(email) match {
          case Some(`password`) => onSuccess(UserProfile(email, database(email))) // ===> Some(p) if p == password
          case Some(_) => onFailure(AuthError("incorrect password."))
          case None => onFailure(AuthError(s"user $email doesn't exist. please sign up."))
        }
      }
  }

  def loginAsZIO(id: String, pw: String): ZIO[Any, LoginService.AuthError, LoginService.UserProfile] =
    ZIO.async[Any, LoginService.AuthError, LoginService.UserProfile] { cb => // callback object created by ZIO
      LoginService.login(id, pw)(
        profile => cb(ZIO.succeed(profile)), // notify the ZIO fiber to complete the ZIO with a success
        error => cb(ZIO.fail(error)) // same, with a failure
      )
    }

  val loginProgram = for {
    email <- Console.readLine("Email: ")
    pass <- Console.readLine("Password: ")
    profile <- loginAsZIO(email, pass).debugThread
    _ <- Console.printLine(s"Welcome to Rock the JVM, ${profile.name}")
  } yield ()

  /**
    * Exercises
    *
    */

  // 2 - lift a future to a ZIO
  // hint: invoke cb when the future complete
  def future2ZIO[A](future: => Future[A])(using ec: ExecutionContext): Task[A] = {
    ZIO.async[Any, Throwable, A] { cb =>
      future onComplete {
        case Success(scc) => cb(ZIO.succeed(scc))
        case Failure(exp) => cb(ZIO.fail(exp))
      }
    }
  }

  lazy val demoFuture2ZIO= {
    given ec: ExecutionContext = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(4))
    val mol = future2ZIO(
      Future {
        println(s"${Thread.currentThread().getName} computing the meaning of life on some thread")
        Thread.sleep(1000)
        42
      }
    )

    mol.debugThread.unit
  }

  // 3 - implement a never-ending ZIO
  def neverEndingZIO[A]: UIO[A] = ZIO.async(_ => ())

  override def run: ZIO[Any, Any, Any] = succeedZIO("computing...") *> ZIO.never *> ZIO.succeed("done")
}