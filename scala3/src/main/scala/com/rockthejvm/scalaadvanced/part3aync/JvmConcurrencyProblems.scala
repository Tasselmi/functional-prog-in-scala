package com.rockthejvm.scalaadvanced.part3aync

object JvmConcurrencyProblems {

  def runInParallel(): Unit = {
    var x = 0

    val thread1 = new Thread( () =>
      x = 1
    )

    val thread2 = new Thread( () =>
      x = 2
    )

    thread1.start()
    thread2.start()
    println(x)
  }

  case class BankAccount(var amount: Int)

  def buy(bankAccount: BankAccount, thing: String, price: Int): Unit = {
    /**
      * this step involves 3 steps:
      *  - read old values
      *  - compute the result
      *  - write the new result
      */
    bankAccount.amount -= price
  }

  def demoBankingProblem(): Unit = {
    (1 to 100000).foreach { nth =>
      val account = BankAccount(50000)
      val thread1 = new Thread(() => buy(account, "shoes", 3000))
      val thread2 = new Thread(() => buy(account, "iPhone", 4000))

      thread1.start()
      thread2.start()
      thread1.join()
      thread2.join()

      if (account.amount != 43000)
        println(s"AHA! After $nth rounds the bank account is broken into amount: ${account.amount}")
    }
  }


  def buySafe(bankAccount: BankAccount, thing: String, price: Int): Unit = {
    bankAccount.synchronized {
      bankAccount.amount -= price //critical section 临界区
    }
  }

  def demoBankingSafe(): Unit = {
    (1 to 200000).foreach { nth =>
      println(nth)
      val account = BankAccount(50000)
      val thread1 = new Thread(() => buySafe(account, "shoes", 3000))
      val thread2 = new Thread(() => buySafe(account, "iPhone", 4000))

      thread1.start()
      thread2.start()
      thread1.join()
      thread2.join()

      if (account.amount != 43000)
        println(s"AHA! After $nth rounds the bank account is broken into amount: ${account.amount}")
    }
  }


  /**
    * Exercises:
    *  1. create "inception threads", each thread prints "hello from thread $i", all message in reverse order
    *  1. what is the max/min value of x ?
    *  1. "sleep fallacy": what is the value of message ?
    */
  // 1
  def inceptionThreads(maxNum: Int, i: Int = 1): Thread = new Thread( () => {
    if (i < maxNum) {
      val newThread = inceptionThreads(maxNum, i + 1)
      newThread.start()
      newThread.join()
    }
    println(s"hello from thread $i")
  })

  // 2  max == 100    min == 1
  def minMax(): Unit = {
    var x = 0
    val threads = (1 to 100).map(_ => new Thread(() => x += 1))
    threads.foreach(_.start())
  }

  // 3
  def demoSleepFallacy(): Unit = {
    var message = ""
    val awesomeThread = new Thread( () =>
      Thread.sleep(1000)
      message = "scala is awesome"
    )

    message = "scala sucks"

    awesomeThread.start()
    Thread.sleep(1001)
    //it is possible the awesomeThread not complete, solution is join it
    awesomeThread.join()
    println(message)
  }


  def main(args: Array[String]): Unit = {
    //runInParallel()
    //demoBankingProblem()
    //demoBankingSafe()

    //inceptionThreads(30).start()
    demoSleepFallacy()
  }
}
