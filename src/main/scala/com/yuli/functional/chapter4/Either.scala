package com.yuli.functional.chapter4

/**
 * either可以定义错误
 */
sealed trait Either[+E, +A] {
  def map[B](f: A => B): Either[E, B]

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B]

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B]

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C]
}

case class Left[+E](value: E) extends Either[E, Nothing] {
  override def map[B](f: Nothing => B): Either[E, B] = ???

  override def flatMap[EE >: E, B](f: Nothing => Either[EE, B]): Either[EE, B] = ???

  override def orElse[EE >: E, B >: Nothing](b: => Either[EE, B]): Either[EE, B] = ???

  override def map2[EE >: E, B, C](b: Either[EE, B])(f: (Nothing, B) => C): Either[EE, C] = ???
}

case class Right[+A](value: A) extends Either[Nothing, A] {
  override def map[B](f: A => B): Either[Nothing, B] = ???

  override def flatMap[EE >: Nothing, B](f: A => Either[EE, B]): Either[EE, B] = ???

  override def orElse[EE >: Nothing, B >: A](b: => Either[EE, B]): Either[EE, B] = ???

  override def map2[EE >: Nothing, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = ???
}