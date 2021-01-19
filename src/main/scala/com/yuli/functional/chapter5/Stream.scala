package com.yuli.functional.chapter5

import scala.annotation.tailrec

/**
 * 描述：
 *
 * @author yuli
 * @date 2020/12/28
 */
sealed trait Stream[+A] {
  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h, _) => Some(h()) //强制对h()求值
  }

  /**
   * 练习5.1
   * Stream转换List
   *
   * @return
   */
  def toList: List[A] = {
    @tailrec
    def loop(stream: Stream[A], list: List[A]): List[A] = stream match {
      case Empty => list.reverse
      case Cons(h, l) => loop(l(), h() :: list)
    }

    loop(this, Nil)
  }

  /**
   * 练习5.2
   * 返回Stream中的前n个元素
   *
   * @param n
   * @return
   */
  def take(n: Int): List[A] = {
    def loop(n: Int, stream: Stream[A]): Stream[A] = stream match {
      case Empty => stream
      case Cons(_, l) if n == 0 => l()
      case Cons(h, l) => loop(n - 1, l(), h())
    }

    loop(n, this, Nil).reverse
  }

  /**
   * 练习5.2
   * 返回Stream中第n个元素后的所有元素
   * @param n
   * @return
   */
  def drop(n: Int): List[A] ={
    def loop(n:Int,stream: Stream[A],list:List[A]):List[A] = stream match {
      case Empty => list
      case Cons(_,l) if n > 0 => loop(n-1,l(),list)
      case Cons(h,l) => loop(n,l(),h()::list)
    }
    loop(n,this,Nil).reverse
  }

}

case object Empty extends Stream[Nothing]

case class Cons[+A](hd: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {

  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A](): Stream[A] = Empty

  def apply[A](as: A*): Stream[A] = if (as.isEmpty) empty() else cons(as.head, apply(as.tail: _*))
}
