package com.yuli.functional.chapter3

/**
 * 树
 *
 * @tparam A
 */
trait Tree[+A]

case class Leaf[A](value: A) extends Tree[A]

case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  /**
   * 练习3.25
   * 返回节点数
   *
   * @return
   */
  def size[A](root: Tree[A]): Int = root match {
    case Leaf(_) => 1
    case Branch(left, right) => 1 + (size(left) + size(right))
  }

  /**
   * 练习3.26
   * 计算树中的最大值
   *
   * @param root
   * @return
   */
  def maximum(root: Tree[Int]): Int = root match {
    case Leaf(value) => value
    case Branch(left, right) => maximum(left) max maximum(right)
  }

  /**
   * 练习3.27
   * 返回跟节点到叶子节点的最大路径长度
   *
   * @param root
   * @tparam A
   * @return
   */
  def depth[A](root: Tree[A]): Int = root match {
    case Leaf(_) => 1
    case Branch(left, right) => 1 + (depth(left) max depth(right))
  }

  /**
   * 练习3.28
   * map函数
   *
   * @param root
   * @param f
   * @tparam A
   * @tparam B
   * @return
   */
  def map[A, B](root: Tree[A])(f: A => B): Tree[B] = root match {
    case Leaf(value) => Leaf(f(value))
    case Branch(left, right) => Branch(map(left)(f), map(right)(f))
  }

  /**
   * 练习3.29
   * 写一个通用fold函数，并实现
   *
   * @param root
   * @param x
   * @param f
   * @param f2
   * @tparam A
   * @tparam B
   * @return
   */
  def fold[A, B](root: Tree[A])(f1: A => B)(f2: (B, B) => B): B = root match {
    case Leaf(value) => f1(value)
    case Branch(left, right) => f2(fold(left)(f1)(f2), fold(right)(f1)(f2))
  }

  def foldSize[A](root: Tree[A]): Int = fold(root)(_ => 1)(_ + _ + 1)

  def foldMaximum(root: Tree[Int]): Int = fold(root)(a => a)((b1, b2) => b1 max b2)

  def foldDepth[A](root: Tree[A]): Int = fold(root)(_ => 1)((b1, b2) => 1 + (b1 max b2))

  def foldMap[A, B](root: Tree[A])(f: A => B): Tree[B] = fold(root)(a => Leaf(f(a)): Tree[B])((b1, b2) => Branch(b1, b2))
}

