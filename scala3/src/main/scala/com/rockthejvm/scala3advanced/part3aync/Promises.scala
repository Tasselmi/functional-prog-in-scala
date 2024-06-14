package com.rockthejvm.`scala3advanced`.part3aync

import scala.concurrent.{Future, Promise}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Try, Success, Failure}

/**
  *  - Futures are read-only async computations
  *  - Promises are controllable wrappers over a Future
  */
object Promises {

  val promise = Promise[Int]()
  val future = promise.future

  // thread 1 -- consumer
  future.onComplete {
    case Success(scc) => println("[consumer] successful with 42")
    case Failure(exp) => exp.printStackTrace()
  }

  // thread 2 -- producer
  val producer = new Thread(() => {
    println("[producer] crunching numbers...")
    Thread.sleep(1000)
    promise.success(42)
    println("[producer] I am done.")
  })

  def main(args: Array[String]): Unit = {
    producer.start()
  }
}
