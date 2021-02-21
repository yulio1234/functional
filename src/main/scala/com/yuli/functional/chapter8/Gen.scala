package com.yuli.functional.chapter8

import com.yuli.functional.chapter6.{RNG, SimpleRNG, State}

case class Gen[A](simple:State[RNG,A])
object Gen{
  def choose(start:Int,stopExclusive:Int):Gen[Int] = Gen[Int](State(SimpleRNG(1).nonNegativeInt).map(n => start + n % (stopExclusive-start)))
}