package com.rockthejvm.zio.part3concurrency

import zio.*
import com.rockthejvm.zio.utils.debugThread

import java.io.File
import java.util.Scanner

object Resources extends ZIOAppDefault {

  //finalizers
  def unsafeMethod(): Int = throw new RuntimeException("not an int here for you")
  val anAttemp = ZIO.attemptUnsafe(_ => unsafeMethod())

  val attemptWithFinalizer = anAttemp.ensuring(ZIO.succeedUnsafe(_ => "finalizer!").debugThread)
  val attemptWith2Finalizer = attemptWithFinalizer.ensuring(ZIO.succeedUnsafe(_ => "another finalizer!").debugThread)


  //resource lifecycle
  class Connection(url: String) {
    def open() = ZIO.succeedUnsafe(_ => s"opening connection to $url...").debugThread
    def close() = ZIO.succeedUnsafe(_ => s"closing connection to $url...").debugThread
  }

  object Connection {
    def create(url: String) = ZIO.succeedUnsafe(_ => new Connection(url))
  }

  val fetchUrl = for {
    conn <- Connection.create("rockthejvm.com")
    fib <- (conn.open() *> ZIO.sleep(300.seconds)).fork
    _ <- ZIO.sleep(1.second) *> ZIO.succeedUnsafe(_ => "interrupting").debugThread *> fib.interrupt
    _ <- fib.join
  } yield ()  //resource leak

  val correctFetchUrl = for {
    conn <- Connection.create("rockthejvm.com")
    fib <- (conn.open() *> ZIO.sleep(300.seconds)).ensuring(conn.close()).fork
    _ <- ZIO.sleep(1.second) *> ZIO.succeedUnsafe(_ => "interrupting").debugThread *> fib.interrupt
    _ <- fib.join
  } yield ()  //resource leak

  //acquireRelease
  val fetchWithResource = for {
    conn <- ZIO.acquireRelease(Connection.create("rockthejvm.com"))(_.close())
    fib <- (conn.open() *> ZIO.sleep(300.seconds)).fork
    _ <- ZIO.sleep(1.second) *> ZIO.succeedUnsafe(_ => "interrupting").debugThread *> fib.interrupt
    _ <- fib.join
  } yield ()


  /**
    * Exercises
    *  1. use the acquireRelease to open a file, print all lines(one every 100 millis), the close the file
    */
  def openFileScanner(path: String): UIO[Scanner] =
    ZIO.succeedUnsafe(_ => new Scanner(new File(path)))

  def readLineByLine(scanner: Scanner): UIO[Unit] =
    if (scanner.hasNext) then
      ZIO.succeed(scanner.nextLine()).debugThread *> ZIO.sleep(100.millis) *> readLineByLine(scanner)
    else
      ZIO.unit

  def acquireOpenFile(path: String) =
    ZIO.succeedUnsafe(_ => s"opening file at $path").debugThread *>
      ZIO.acquireReleaseWith(
        openFileScanner(path)
      )(
        sc => ZIO.succeedUnsafe(_ => s"closing file at $path").debugThread *> ZIO.succeedUnsafe(_ => sc.close())
      )(
        sc => readLineByLine(sc)
      )

  val testInterruptFileDisplay = for {
    fib <- acquireOpenFile("scala3/src/main/scala/com/rockthejvm/zio/part3concurrency/Resources.scala").fork
    _ <- ZIO.sleep(2.seconds) *> fib.interrupt
  } yield ()

  /**
    * acquireRelease VS acquireReleaseWith
    */
  // nested resource
  def connFromConfig(path: String): UIO[Unit] = ZIO.scoped {
    for {
      scanner <- ZIO.acquireRelease(openFileScanner(path))(
        scanner => ZIO.succeedUnsafe(_ => "closing file").debugThread *> ZIO.succeedUnsafe(_ => scanner.close()))
      conn <- ZIO.acquireRelease(Connection.create(scanner.nextLine()))(_.close())
      _ <- conn.open() *> ZIO.never
    } yield ()
  }

  override def run: ZIO[Any, Any, Any] = connFromConfig("scala3/src/main/resources/words/file01.txt")
}
