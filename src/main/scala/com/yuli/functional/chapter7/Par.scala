package com.yuli.functional.chapter7

import java.util.concurrent.{Callable, ExecutorService, Future, TimeUnit}

object Par {
  type Par[A] = ExecutorService => Future[A]

  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

  def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a)

  def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] = {
    (es: ExecutorService) => {
      val af = a(es)
      val bf = b(es)
      UnitFuture(f(af.get, bf.get))
    }
  }

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  /**
   * 练习7.4
   * 返回一个函数
   * @param f
   * @tparam A
   * @tparam B
   * @return
   */
  def asyncF[A, B](f: A => B): A => Par[B] = a => lazyUnit(f(a))

  /**
   * 继续返回一个函数
   *
   * @param a
   * @tparam A
   * @return
   */
  def fork[A](a: => Par[A]): Par[A] = es => es.submit(() => a(es).get())

  private case class UnitFuture[A](get: A) extends Future[A] {
    override def cancel(b: Boolean): Boolean = false

    override def isCancelled: Boolean = false

    override def isDone: Boolean = true

    override def get(l: Long, timeUnit: TimeUnit): A = get
  }

  def map[A,B](pa:Par[A])(f:A=>B):Par[B] = map2(pa,unit(()))((a,_)=>f(a))
  def sortPar(parList:Par[List[Int]]):Par[List[Int]] = map(parList)(_.sorted)

  /**
   * 练习7.5
   * @param ps
   * @tparam A
   * @return
   */
  def sequence[A](ps:List[Par[A]]):Par[List[A]] = ps.foldRight[Par[List[A]]](unit(Nil))((a,b)=>map2(a,b)((a,b)=>a::b))
  def parMap[A,B](ps:List[A])(f:A=>B) :Par[List[B]] = fork{
    val fbs:List[Par[B]] = ps.map(asyncF(f))
    sequence(fbs)
  }

  def parFilter[A](as:List[A])(f:A=>Boolean):Par[List[A]] = as.foldRight[List[A]](Nil)((a,b)=>map())
}
