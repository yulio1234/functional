package com.yuli.functional.chapter5

import com.yuli.functional.chapter5.Stream.{cons, empty, unfold}

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
   * 练习5.3返回连续满足断言的函数，遇到不满足就中断
   *
   * @param p
   * @return
   */
  def takeWhile(p: A => Boolean): Stream[A] = {
    def loop(stream: Stream[A], newStream: Stream[A]): Stream[A] = stream match {
      case _ => newStream
      case Cons(h, l) if p(h()) => Stream.cons(h(), loop(l(), newStream))
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
   *
   * @param p
   * @return
   */
  def forAll(p: A => Boolean): Boolean = foldRight(true)((a, b) => p(a) && b)

  /**
   * 练习5.5
   *
   * @param p
   * @return
   */
  def takeWhileFold(p: A => Boolean): Stream[A] = foldRight(Empty: Stream[A])((a, b) => if (p(a)) Stream.cons(a, b) else Empty)

  /**
   * 练习5.6
   *
   * @return
   */
  def headOptionFold: Option[A] = foldRight(None: Option[A])((a, _) => if (a == Empty) None else Some(a))

  /**
   * 练习5.7
   * 用rightFold实现map
   *
   * @param f
   * @tparam B
   * @return
   */
  def map[B](f: A => B): Stream[B] = foldRight(Empty: Stream[B])((a, b) => Stream.cons(f(a), b))

  def filter(p: A => Boolean): Stream[A] = foldRight(Empty: Stream[A])((a, b) => if (p(a)) Stream.cons(a, b) else b)

  def append[B >: A](s: => Stream[B]): Stream[B] = foldRight(s)((a, b) => Stream.cons(a, b))

  def flatMap[B](f: A => Stream[B]): Stream[B] = foldRight(Empty: Stream[B])((a, b) => f(a) append b)

  /**
   * 因为是惰性求值,所有不会对匹配到后的Stream求值.
   *
   * @param p
   * @return
   */
  def find(p: A => Boolean): Option[A] = filter(p).headOptionFold

  /**
   * 练习5.13
   *
   * @param f
   * @tparam B
   * @return
   */
  def mapViaUnfold[B](f: A => B): Stream[B] = Stream.unfold(this) {
    case Empty => None
    case Cons(h, t) => Some(f(h()), t())
  }

  def takeViaUnfold(n: Int): Stream[A] = Stream.unfold((n, this)) {
    case (n, Cons(h, t)) if n > 0 => Some(h(), (n - 1, t()))
    case (_, Empty) => None
    case (0, Cons(h, t)) => None
  }

  def takeWhileViaUnfold(p: A => Boolean): Stream[A] = Stream.unfold(this) {
    case Cons(h, t) if p(h()) => Some(h(), t())
    case _ => None
  }

  def zipWith[B, C](s: Stream[B])(f: (A, B) => C): Stream[C] = unfold((this, s)) {
    case (Cons(h1, t1), Cons(h2, t2)) => Some((f(h1(), h2()), (t1(), t2())))
    case _ => None
  }

  def zipAll[B](s: Stream[B]): Stream[(Option[A], Option[B])] = unfold((this, s)) {
    case (Cons(h1, t1), Cons(h2, t2)) => Some((Some(h1()), Some(h2())), (t1(), t2()))
    case (Empty, Cons(h2, t2)) => Some((None, Some(h2())), (Empty, t2()))
    case (Cons(h1, t1), Empty) => Some((Some(h1()), None), (t1(), Empty))
    case (Empty, Empty) => None
  }

  /**
   * 练习5.14
   * 检查一个stream是否是另一个stream的前缀
   *
   * @param s
   * @tparam A
   * @return
   */
  def startsWith[A](s: Stream[A]): Boolean = zipAll(s).takeWhileViaUnfold(!_._2.isEmpty).forAll {
    case (h1, h2) => h1 == h2
  }

  /**
   * 练习5.15
   * 用unfold实现tails，返回所有后缀
   *
   * @return
   */
  def tails: Stream[Stream[A]] = unfold(this) {
    case s@Cons(_, t) => Some(s, t())
    case _ => None
  }.append(Empty)

  /**
   * 是否包含子序列
   *
   * @param s
   * @tparam A
   * @return
   */
  def hasSubsequence[A](s: Stream[A]): Boolean = tails exists (_ startsWith s)

}

case object Empty extends Stream[Nothing]

case class Cons[+A](hd: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {

  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  /**
   * 练习5.8
   * 根据给定值返回一个无限流
   *
   * @param a
   * @tparam A
   * @return
   */
  def constant[A](a: A): Stream[A] = {
    lazy val ones: Stream[A] = cons(a, ones)
    ones
  }

  /**
   * 练习5.9
   *
   * @param n
   * @return
   */
  def from(n: Int): Stream[Int] = cons(n, from(n + 1))

  /**
   * 练习5.10
   * 生成斐波那契数列
   *
   * @return
   */
  def fibs: Stream[Int] = {
    def loop(n1: Int, n2: Int): Stream[Int] = {
      cons(n1, loop(n2, n1 + n2))
    }

    loop(0, 1)
  }

  /**
   * 练习5.11
   * 更加通用的构造流函数，它接受一个初始状态，以及一个在生成的Stream中用于产生下一个状态和下一个值的函数
   *
   * @param z
   * @param f
   * @tparam A
   * @tparam S
   * @return
   */
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case None => Empty
    case Some((a, s)) => cons(a, unfold(s)(f))
  }

  /**
   * 练习5.13通过unfold实现fibs,from,constant
   *
   * @return
   */
  def fibsUnfold: Stream[Int] = unfold((0, 1))(a => Some(a._1, (a._2, a._1 + a._2)))

  def constantUnfold(n: Int): Stream[Int] = unfold(n)(s => Some(s, s))

  def fromUnfold(n: Int): Stream[Int] = unfold(n)(s => Some(s + 1, s))

  def empty[A](): Stream[A] = Empty

  def apply[A](as: A*): Stream[A] = if (as.isEmpty) empty() else cons(as.head, apply(as.tail: _*))
}
