package com.yuli.functional.chapter5

import scala.annotation.tailrec

/**
 * 描述：惰性流
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
  def take(n: Int): Stream[A] = {
    def loop(n: Int, stream: Stream[A], takeStream: Stream[A]): Stream[A] = stream match {
      case Empty => takeStream
      case Cons(_, l) if n == 0 => takeStream
      case Cons(h, l) => Stream.cons(h(), loop(n - 1, l(), takeStream));
    }

    loop(n, this, Empty)
  }


  /**
   * 练习5.2
   * 返回Stream中第n个元素后的所有元素
   *
   * @param n
   * @return
   */
  def drop(n: Int): Stream[A] = {
    def loop(n: Int, stream: Stream[A], newStream: Stream[A]): Stream[A] = stream match {
      case Empty => newStream
      case Cons(_, l) if n > 0 => loop(n - 1, l(), newStream)
      case Cons(h, l) => Stream.cons(h(), loop(n, l(), newStream))
    }

    loop(n, this, Empty)
  }

  /**
   * 练习5.3返回满足断言的函数
   *
   * @param p
   * @return
   */
  def takeWhile(p: A => Boolean): Stream[A] = {
    def loop(stream: Stream[A], newStream: Stream[A]): Stream[A] = stream match {
      case Empty => newStream
      case Cons(h, l) if p(h()) => Stream.cons(h(), loop(l(), newStream))
      case Cons(_, l) => loop(l(), newStream)
    }

    loop(this, Empty)
  }

  /**
   * 检查元素流中是否存在匹配表达式的值
   * 如果||前的函数执行正确，就提前返回
   *
   * @param p
   * @return
   */
  def exists(p: A => Boolean): Boolean = this match {
    case Cons(h, t) => p(h()) || t().exists(p)
    case _ => false
  }

  /**
   * 右折叠，函数=>B 不对第二个参数进行求值，如果不求值就不会发生递归
   *
   * @param z
   * @param f
   * @tparam B
   * @return
   */
  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(h, t) => f(h(), t().foldRight(z)(f))
    case _ => z
  }

  /**
   * 用foldRight实现exists
   *
   * @param p
   * @return
   */
  def existsFoldRight(p: A => Boolean): Boolean = foldRight(false)((a, b) => p(a) || b)

  /**
   * 练习5.4
   * 检查Stream中所有元素是否与给定的断言匹配。如果不匹配就直接返回。
   * 因为如果不匹配需要立即返回，所有b不能被求值，需要在&&之前就返回
   * @param p
   * @return
   */
  def forAll(p: A => Boolean): Boolean = foldRight(true)((a, b) => p(a) && b)

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
