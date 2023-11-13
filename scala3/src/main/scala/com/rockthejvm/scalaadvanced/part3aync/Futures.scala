package com.rockthejvm.scalaadvanced.part3aync

import java.util.concurrent.Executors
import scala.concurrent.{ExecutionContext, ExecutionContextExecutorService, Future, Await}
import scala.concurrent.duration.*
import scala.util.{Failure, Success, Try}

object Futures {

  ////////////////////////////////////////////////////////////////////////////////
  //  1. introduction
  ////////////////////////////////////////////////////////////////////////////////
  def calcaluteSomething(): Int = {
    Thread.sleep(1000L)
    42
  }

  val executor = Executors.newFixedThreadPool(4)
  given executionContext: ExecutionContextExecutorService = ExecutionContext.fromExecutorService(executor)

  val aFuture = Future.apply(calcaluteSomething())

  //one argument method sugar + partial function
  aFuture.onComplete {
    case Success(value) => println(s"I've completed calculation $value")
    case Failure(exp) => println(exp.getMessage)
  }


  ////////////////////////////////////////////////////////////////////////////////
  //  2. composition of futures
  ////////////////////////////////////////////////////////////////////////////////
  case class Profile(name: String)


  ////////////////////////////////////////////////////////////////////////////////
  //  3. block of futures
  ////////////////////////////////////////////////////////////////////////////////
  val future = Future {
    Thread.sleep(3000)
    println("I am awake")
    999
  }

  // trait Future[+T] extends Awaitable[T]
  // Await may cause java.util.concurrent.TimeoutException
  val finalResut = Try(Await.result(future, 1.seconds))
  finalResut match {
    case Success(scc) => println(scc)
    case Failure(exp) => exp.printStackTrace()
  }


  def main(args: Array[String]): Unit = {
    //println(aFuture.value)

    Thread.sleep(2000L)
    executor.shutdown()
  }
}
