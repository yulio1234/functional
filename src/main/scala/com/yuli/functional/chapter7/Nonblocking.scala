package com.yuli.functional.chapter7

import java.util.concurrent.{Callable, CountDownLatch, ExecutorService}
import java.util.concurrent.atomic.AtomicReference

object Nonblocking {

  sealed trait Future[A] {
    private[chapter7] def apply(k: A => Unit): Unit
  }

  type Par[+A] = ExecutorService => Future[A]

  def run[A](es: ExecutorService)(p: Par[A]): A = {
    val ref = new AtomicReference[A]()
    val latch = new CountDownLatch(1)
    p(es) { a =>
      ref.set(a);
      latch.countDown()
    }
    latch.await()
    ref.get()
  }

  def unit[A](a: A): Par[A] = es => new Future[A] {
    override private[chapter7] def apply(k: A => Unit): Unit = k(a)
  }

  def fork[A](a: => Par[A]): Par[A] = es => new Future[A] {
    override private[chapter7] def apply(k: A => Unit): Unit = eval(es)(a(es)(k))
  }

  def eval(es: ExecutorService)(r: => Unit): Unit = es.submit(new Callable[Unit] {
    override def call(): Unit = r
  })

  def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] = es => new Future[C] {
    override private[chapter7] def apply(k: C => Unit): Unit = {
      var ar: Option[A] = None
      var br: Option[B] = None
      val combiner = Actor[Either[A, B]](es) {
        case Left(a) => br match {
          case None => ar = Some(a)
          case Some(b) => eval(es)(k(f(a, b)))
        }
        case Right(b) => ar match {
          case None => br = Some(b)
          case Some(a) => eval(es)(k(f(a, b)))
        }
      }
      a(es)(a => combiner ! Left(a))
      b(es)(b => combiner ! Right(b))
    }
  }


}
