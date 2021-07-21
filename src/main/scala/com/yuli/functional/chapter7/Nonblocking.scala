package com.yuli.functional.chapter7

import java.util.concurrent.{Callable, CountDownLatch, ExecutorService}
import java.util.concurrent.atomic.AtomicReference

object Nonblocking {

  sealed trait Future[A] {
    private[chapter7] def apply(k: A => Unit): Unit
  }

  type Par[A] = ExecutorService => Future[A]

  object Par {

    def run[A](es: ExecutorService)(p: Par[A]): A = {
      val ref = new AtomicReference[A]()
      val latch = new CountDownLatch(1)
      //柯里化，传入一个es，返回future再继续传入方法
      p(es) { a =>
        ref.set(a);
        latch.countDown()
      }
      latch.await()
      ref.get()
    }

    def unit[A](a: A): Par[A] = (_: ExecutorService) => new Future[A] {
      override private[chapter7] def apply(k: A => Unit): Unit = k(a)
    }

    def fork[A](a: => Par[A]): Par[A] = es => new Future[A] {
      override private[chapter7] def apply(k: A => Unit): Unit = eval(es)(a(es)(k))
    }

    def eval(es: ExecutorService)(r: => Unit): Unit = es.submit(new Callable[Unit] {
      override def call(): Unit = r
    })

    def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

    def asyncF[A, B](f: A => B): A => Par[B] = a => lazyUnit(f(a))

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

    def map[A, B](p: Par[A])(f: A => B): Par[B] =
      es => new Future[B] {
        override def apply(k: B => Unit): Unit =
          p(es)(a => eval(es) {
            k(f(a))
          })
      }

    def flatMap[A, B](p: Par[A])(f: A => Par[B]): Par[B] = es => new Future[B] {
      override private[chapter7] def apply(k: B => Unit): Unit = p(es)(a => f(a)(es)(k))
    }
    //    def join[A](p:Par[Par[A]])

    def sequenceBalanced[A](as: IndexedSeq[Par[A]]): Par[IndexedSeq[A]] = fork {
      if (as.isEmpty) unit(Vector())
      else if (as.length == 1) map(as.head)(a => Vector(a))
      else {
        val (l, r) = as.splitAt(as.length / 2)
        map2(sequenceBalanced(l), sequenceBalanced(r))(_ ++ _)
      }
    }

    def parMap[A, B](as: IndexedSeq[A])(f: A => B): Par[IndexedSeq[B]] = sequenceBalanced(as.map(asyncF(f)))

    implicit def toParOps[A](p: Par[A]): ParOps[A] = new ParOps(p)

    class ParOps[A](p: Par[A]) {
      def map[B](f: A => B): Par[B] = Par.map(p)(f)

      def map2[B, C](b: Par[B])(f: (A, B) => C): Par[C] = Par.map2(p, b)(f)

      def flatMap[B](f: A => Par[B]): Par[B] = Par.flatMap(p)(f)
    }
  }


}
