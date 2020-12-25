package com.yuli.functional.chapter4

/**
 * 描述：
 *
 * @author yuli
 * @date 2020/12/24
 */
sealed trait Option[+A] {
  /**
   * 如果Option不为None，对其应用f
   *
   * @param f
   * @tparam B
   * @return
   */
  def map[B](f: A => B): Option[B];

  /**
   * 如果Option不为None，对其应用f，可能会失败
   *
   * @param f
   * @tparam B
   * @return
   */
  def flatMap[B](f: A => Option[B]): Option[B]

  /**
   *
   * @param default
   * @tparam B
   * @return
   */
  def getOrElse[B >: A](default: => B): B

  def orElse[B >: A](op: => Option[B]): Option[B]
  def filter(f:A => Boolean):Option[A]
}

case class Some[+A](get: A) extends Option[A]{
  /**
   * 如果Option不为None，对其应用f
   *
   * @param f
   * @tparam B
   * @return
   */
  override def map[B](f: A => B): Option[B] = Some(f(get))

  /**
   * 如果Option不为None，对其应用f，可能会失败
   *
   * @param f
   * @tparam B
   * @return
   */
  override def flatMap[B](f: A => Option[B]): Option[B] = f(get)

  /**
   *
   * @param default
   * @tparam B
   * @return
   */
  override def getOrElse[B >: A](default: => B): B = get

  override def orElse[B >: A](op: => Option[B]): Option[B] = Some(get)

  override def filter(f: A => Boolean): Option[A] = if (f(get)) Some(get) else None
}

case object None extends Option[Nothing]{
  /**
   * 如果Option不为None，对其应用f
   *
   * @param f
   * @tparam B
   * @return
   */
  override def map[B](f: Nothing => B): Option[B] = None

  /**
   * 如果Option不为None，对其应用f，可能会失败
   *
   * @param f
   * @tparam B
   * @return
   */
  override def flatMap[B](f: Nothing => Option[B]): Option[B] = None

  /**
   *
   * @param default
   * @tparam B
   * @return
   */
  override def getOrElse[B >: Nothing](default: => B): B = default

  override def orElse[B >: Nothing](op: => Option[B]): Option[B] = op

  override def filter(f: Nothing => Boolean): Option[Nothing] = None
}

