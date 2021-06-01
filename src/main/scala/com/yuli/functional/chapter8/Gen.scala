package com.yuli.functional.chapter8

import com.yuli.functional.chapter6.{RNG, SimpleRNG, State}

case class Gen[A](simple:State[RNG,A]){
  def flatMap[B](f:A=>Gen[B]):Gen[B] = Gen(simple.flatMap(a=>f(a).simple))
  def listOfN(size:Gen[Int]):Gen[List[A]] = size.flatMap(s=>Gen(State.sequence(List.fill(s)(this.simple))))

}
object Gen{
  /**
   * 练习8.4
   * @param start
   * @param stopExclusive
   * @return
   */
  def choose(start:Int,stopExclusive:Int):Gen[Int] = Gen[Int](State(SimpleRNG(1).nonNegativeInt).map(n => start + n % (stopExclusive-start)))

  /**
   * 练习8.5
   * @param a
   * @tparam A
   * @return
   */
  def unit[A](a: => A):Gen[A] = Gen(State.unit(a))
  def boolean:Gen[Boolean] = Gen(State(SimpleRNG(1).boolean))
  def listOfN[A](n:Int,g:Gen[A]):Gen[List[A]] = Gen(State.sequence(List.fill(n)(g.simple)))
  def union[A](g1:Gen[A],g2:Gen[A]):Gen[A] =
}