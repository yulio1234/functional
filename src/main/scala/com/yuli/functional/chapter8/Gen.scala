package com.yuli.functional.chapter8

import com.yuli.functional.chapter6.{RNG, SimpleRNG, State}

case class Gen[A](simple: State[RNG, A]) {
  def map[B](f: A => B): Gen[B] = Gen(simple.map(f))

  def map2[B, C](g: Gen[B])(f: (A, B) => C): Gen[C] = Gen(simple.map2(g.simple)(f))

  def **[B](g: Gen[B]): Gen[(A, B)] = (this map2 g) ((_, _))

  /**
   * 练习8.6
   *
   * @return
   */
  def flatMap[B](f: A => Gen[B]): Gen[B] = Gen(simple.flatMap(a => f(a).simple))

  def listOfN(size: Gen[Int]): Gen[List[A]] = size.flatMap(s => Gen(State.sequence(List.fill(s)(this.simple))))

  def listOfN(size: Int): Gen[List[A]] = Gen.listOfN(size, this)

  /**
   * 练习8.13
   *
   * @param size
   * @return
   */
  def listOf1(size: Int): Gen[List[A]] = Gen.listOfN(if (size < 1) 1 else size, this)

  /**
   * 将Gen转换为SGen
   *
   * @return
   */
  def unSized: SGen[A] = SGen(_ => this)
}

case class SGen[A](forSize: Int => Gen[A]) {
  def apply(size: Int): Gen[A] = forSize(size)

  def map[B](f: A => B): SGen[B] = SGen(forSize(_).map(f))

  /**
   * 练习8.11
   *
   * @param f
   * @tparam B
   * @return
   */
  def flatMap[B](f: A => SGen[B]): SGen[B] = {
    val g2: Int => Gen[B] = n => {
      forSize(n).flatMap(f(_).forSize(n))
    }
    SGen(g2)
  }

}

object Gen {
  /**
   * 练习8.4
   *
   * @param start
   * @param stopExclusive
   * @return
   */
  def choose(start: Int, stopExclusive: Int): Gen[Int] = Gen[Int](State(RNG.nonNegativeInt).map(n => start + n % (stopExclusive - start)))

  /**
   * 练习8.5
   *
   * @param a
   * @tparam A
   * @return
   */
  def unit[A](a: => A): Gen[A] = Gen(State.unit(a))


  def boolean: Gen[Boolean] = Gen(State(RNG.boolean))

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] = Gen(State.sequence(List.fill(n)(g.simple)))


  /**
   * 练习8.12用
   *
   * @param g
   * @tparam A
   * @return
   */
  def listOf[A](g: Gen[A]): SGen[List[A]] = SGen(g.listOf1)

  /**
   * 练习8.7
   *
   * @param g1
   * @param g2
   * @tparam A
   * @return
   */
  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] = boolean.flatMap(b => if (b) g1 else g2)

  /**
   * 练习8.8
   *
   * @param g1
   * @param g2
   * @tparam A
   * @return
   */
  def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] = {
    val value = g1._2.abs / g1._2.abs + g2._2.abs
    Gen(State(RNG.double).flatMap(d => if (d < value) g1._1.simple else g2._1.simple))
  }

}