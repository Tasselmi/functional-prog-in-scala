package com.rockthejvm.`scala3advanced`.part3aync

import java.util.concurrent.Executors

object JvmConcurrencyIntro {

  ////////////////////////////////////////////////////////////////////////////////
  //  1. jvm thread
  ////////////////////////////////////////////////////////////////////////////////
  def basicThread(): Unit = {
    val runnable = new Runnable {
      override def run(): Unit = println("running on some thread")
    }

    //threads on the JVM
    val aThread = new Thread(runnable)
    aThread.start()
    //JVM thread == OS thread (soon to change via project Loom)

    aThread.join()  //block until thread finishes
  }

  basicThread()

  //order not guaranteed
  def orderOfExecution(): Unit = {
    val threadHello = new Thread( () => (1 to 3).foreach(_ => println("hello")) )
    val threadGoodbye = new Thread( () => (1 to 3).foreach(_ => println("goodbye")) )
    threadHello.start()
    threadGoodbye.start()
  }

  orderOfExecution()

  //executors
  def demoExecutors(): Unit = {
    val threadPool = Executors.newFixedThreadPool(3)
    threadPool.execute( () => println("something in the thread pool") )

    threadPool.execute { () =>
      println("second task")
    }

    threadPool.execute { () =>
      Thread.sleep(1000L)
      println("third task")
    }

    threadPool.shutdown()
  }

  demoExecutors()


  def main(args: Array[String]): Unit = {

  }
}
