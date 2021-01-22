package com.yuli.functional.chapter4

/**
 * either可以定义错误
 */
sealed trait Either[+E, +A] {
  def map[B](f: A => B): Either[E, B]

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B]

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B]

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C]

  /**
   * 练习4.7
   * @param es
   * @tparam E
   * @tparam A
   * @return
   */
  def sequence[E,A](es:List[Either[E,A]]):Either[E,List[A]] = es.foldRight(Right(Nil):Either[E,List[A]])((a,b)=>a.map2(b)(_::_))
  def traverse[E,A,B](as:List[A])(f: A=>Either[E,B]):Either[E,List[B]] = as.foldRight(Right(Nil):Either[E,List[B]])((a,b)=>f(a).map2(b)(_::_))
}

case class Left[+E](value: E) extends Either[E, Nothing] {
  override def map[B](f: Nothing => B): Either[E, B] = this

  override def flatMap[EE >: E, B](f: Nothing => Either[EE, B]): Either[EE, B] = this

  override def orElse[EE >: E, B >: Nothing](b: => Either[EE, B]): Either[EE, B] = b

  override def map2[EE >: E, B, C](b: Either[EE, B])(f: (Nothing, B) => C): Either[EE, C] = this
}

case class Right[+A](value: A) extends Either[Nothing, A] {
  override def map[B](f: A => B): Either[Nothing, B] = Right(f(value))

  override def flatMap[EE >: Nothing, B](f: A => Either[EE, B]): Either[EE, B] = f(value)

  override def orElse[EE >: Nothing, B >: A](b: => Either[EE, B]): Either[EE, B] = this

  override def map2[EE >: Nothing, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = b match {
    case Left(e)=> Left(e)
    case Right(v) => Right(f(value,v))
  }
}