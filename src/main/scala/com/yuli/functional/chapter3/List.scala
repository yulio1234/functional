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
case class Cons[+A](head:A,tail:List[A]) extends List[A]
object List{
  /**
   * 求集合的总和
   * @param ints
   * @return
   */
  def sum(ints:List[Int]):Int = ints match {
    case Nil => 0
    case Cons(x,xs) => x+sum(xs)
  }

  /**
   * 求集合的乘积
   * @param ds
   * @return
   */
  def product(ds:List[Double]):Double = ds match {
    case Nil => 1.0
    case Cons(0.0,_)=>0.0
    case Cons(x,xs)=> x * product(xs)
  }

  /**
   * 练习3.4 删除第一个元素
   * @param list
   * @tparam A
   * @return
   */
  def tail[A](list:List[A]): List[A] = list match {
    case Nil => Nil
    case Cons(_,xs) => xs
  }
  def setHead[A](z:A,list: List[A]):Cons[A]=list match {
    case Nil => Cons(z,Nil)
    case Cons(_,xs) => Cons(z,xs)
  }

  /**
   * 练习3.4 删除前N个元素
   * @param x
   * @param list
   * @tparam A
   * @return
   */
  @tailrec
  def drop[A](x:Int,list: List[A]) :List[A] = {
    if (x == 0 || list == Nil) list
    else drop(x-1,tail(list))

  }

  /**
   * 练习3.5 去除相同的前缀数据
   * @param l
   * @param f
   * @tparam A
   * @return
   */
  def dropWhile[A](l:List[A])(f:A => Boolean):List[A] =  l match {
    case Cons(h,t) if f(h) => dropWhile(t)(f)
    case _ => l
  }

  /**
   * 练习3.6 删除尾部数据
   * @param l
   * @tparam A
   * @return
   */
  def init[A](l:List[A]):List[A]= l match {
    case Cons(_,xs) if xs == Nil => Nil
    case Cons(x,xs) => Cons(x,init(xs))
  }

  /**
   * 尾部追加数据
   * @param a1
   * @param a2
   * @tparam A
   * @return
   */
  def append[A] (a1:List[A],a2:List[A]) :List[A] = a1 match {
    case Nil => a2
    case Cons(h,t) => Cons(h,append(t,a2))
  }

  /**
   * 右折叠，将计算方式泛化
   * @param as
   * @param z
   * @param f
   * @tparam A
   * @tparam B
   * @return
   */
  def foldRight[A,B](as:List[A],z:B)(f:(A,B)=>B) :B = as match {
    case Nil => z
    case Cons(x,xs) => f(x,foldRight(xs,z)(f))
  }

  /**
   * 左折叠
   * @param as
   * @param z
   * @param f
   * @tparam A
   * @tparam B
   * @return
   */
  def foldLeft[A,B](as:List[A],z:B)(f:(B,A) =>B):B = as match {
    case Nil => z
    case Cons(x,xs) => f(foldLeft(xs,z)(f),x)
  }

  def flip[A](as:List[A]) :List[A] = as match {
    case 
  }
  /**
   * 通过左折叠累加
   * @param ns
   * @return
   */
  def sum2(ns:List[Int]) :Int= foldRight(ns,0)((x,y) => x+y)
  def sum3(ns:List[Int]) :Int = foldLeft(ns,0)(_+_)
  /**
   * 通过左折叠累乘
   * @param ns
   * @return
   */
  def product2(ns:List[Double]):Double = foldRight(ns,1.0)(_*_)
  def product3(ns:List[Double]):Double = foldLeft(ns,1.0)(_*_)

  /**
   * 练习3.9 通过foldRight计算数组长度
   * @param as
   * @return
   */
  def length[A](as:List[A]):Int = foldRight(as,0)((_,z) => z+1)
  def length[A](as:List[A]):Int = foldLeft(as,0)((z,_) => z+1)
  def apply[A](as:A*): List[A] = if (as.isEmpty) Nil else Cons(as.head,apply(as.tail:_*))
}