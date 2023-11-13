package com.rockthejvm.zio.utils

import zio.{ZIO, Unsafe, UIO}

extension [R, E, A](zio: ZIO[R, E, A]) {
  def debugThread: ZIO[R, E, A] = zio
    .tap(a => ZIO.succeed(println(s"${Thread.currentThread().getName} $a")))
    .tapErrorCause(cause => ZIO.succeed(println(s"${Thread.currentThread().getName}[FAIL] $cause")))
}

def succeedZIO[A](in: => A): UIO[A] = ZIO.succeed(in).debugThread