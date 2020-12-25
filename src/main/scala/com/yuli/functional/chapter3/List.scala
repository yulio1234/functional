package com.yuli.functional.chapter3

import scala.annotation.tailrec

sealed trait List[+A]

/**
 *
 *
 * @author yuli
 * @date 2020/7/28
 */
case object Nil extends List[Nothing]

case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  /**
   * 求集合的总和
   *
   * @param ints
   * @return
   */
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  /**
   * 求集合的乘积
   *
   * @param ds
   * @return
   */
  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  /**
   * 练习3.4 删除第一个元素
   *
   * @param list
   * @tparam A
   * @return
   */
  def tail[A](list: List[A]): List[A] = list match {
    case Nil => Nil
    case Cons(_, xs) => xs
  }

  def setHead[A](z: A, list: List[A]): Cons[A] = list match {
    case Nil => Cons(z, Nil)
    case Cons(_, xs) => Cons(z, xs)
  }

  /**
   * 练习3.4 删除前N个元素
   *
   * @param x
   * @param list
   * @tparam A
   * @return
   */
  @tailrec
  def drop[A](x: Int, list: List[A]): List[A] = {
    if (x == 0 || list == Nil) list
    else drop(x - 1, tail(list))

  }

  /**
   * 练习3.5 去除相同的前缀数据
   *
   * @param l
   * @param f
   * @tparam A
   * @return
   */
  def dropWhile[A](l: List[A])(f: A => Boolean): List[A] = l match {
    case Cons(h, t) if f(h) => dropWhile(t)(f)
    case _ => l
  }

  /**
   * 练习3.6 删除尾部数据
   *
   * @param l
   * @tparam A
   * @return
   */
  def init[A](l: List[A]): List[A] = l match {
    case Cons(_, xs) if xs == Nil => Nil
    case Cons(x, xs) => Cons(x, init(xs))
  }

  /**
   * 尾部追加数据
   *
   * @param a1
   * @param a2
   * @tparam A
   * @return
   */
  def append[A](a1: List[A], a2: List[A]): List[A] = a1 match {
    case Nil => a2
    case Cons(h, t) => Cons(h, append(t, a2))
  }

  /**
   * 右折叠，将计算方式泛化
   *
   * @param as
   * @param z
   * @param f
   * @tparam A
   * @tparam B
   * @return
   */
  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => f(x, foldRight(xs, z)(f))
  }

  /**
   * 练习3.9 通过foldRight计算数组长度
   *
   * @param as
   * @return
   */
  def lengthFoldRight[A](as: List[A]): Int = foldRight(as, 0)((_, z) => z + 1)

  /**
   * 练习3.10 尾递归左折叠
   *
   * @param as
   * @param z
   * @param f
   * @tparam A
   * @tparam B
   * @return
   */
  @tailrec
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
  }

  /**
   * 练习3.11
   *
   * @param ns
   * @return
   */
  def sumFoldRight(ns: List[Int]): Int = foldRight(ns, 0)((x, y) => x + y)

  def sumFoldLeft(ns: List[Int]): Int = foldLeft(ns, 0)(_ + _)

  def productFoldRight(ns: List[Double]): Double = foldRight(ns, 1.0)(_ * _)

  def productFoldLeft(ns: List[Double]): Double = foldLeft(ns, 1.0)(_ * _)

  def lengthFoldLeft[A](as: List[A]): Int = foldLeft(as, 0)((z, _) => z + 1)

  /**
   * 练习3.12
   * 通过foldLeft实现集合翻转
   *
   * @param as
   * @tparam A
   * @return
   */
  def flip[A](as: List[A]): List[A] = foldLeft(as, Nil: List[A])((x, z) => Cons(z, x))

  /**
   * 练习3.13
   * 通过foldRight来写foldLeft
   * 通过foldLeft来写foldRight
   *
   * @param as
   * @param z
   * @param f
   * @tparam A
   * @tparam B
   * @return
   */
  def foldRightByLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = foldLeft(flip(as), z)(f)

  def foldLeftByRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = foldRight(flip(as), z)(f)

  /**
   * 练习3.14
   * 在列表的尾部添加列表
   *
   * @param a1
   * @param a2
   * @tparam A
   * @return
   */
  def appendFold[A](a1: List[A], a2: List[A]): List[A] = foldRightByLeft(a1: List[A], a2: List[A])((a, b) => Cons(b, a))

  /**
   * 练习3.15
   * 将一组列表连接成单个列表
   *
   * @param lists
   * @tparam A
   * @return
   */
  def mergeLists[A](lists: List[List[A]]): List[A] = foldRight(lists, Nil: List[A])((a, b) => appendFold(a, b))



  //  /**
  //   * 练习3。16
  //   * @param list
  //   * @return
  //   */
  //  def plusOne(list: List[Int]): List[Int] = ...

  def apply[A](as: A*): List[A] = if (as.isEmpty) Nil else Cons(as.head, apply(as.tail: _*))
}