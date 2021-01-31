package com.yuli.functional.chapter6

import com.yuli.functional.chapter6.RNGTest.rng

object RNGTest extends App {
  private val rng: SimpleRNG = SimpleRNG(42)
  private val nextRNG: (Int, RNG) = rng.nextInt
  println(nextRNG._1)
  private val nextInt: (Int, RNG) = nextRNG._2.nextInt
  println(nextInt._1)
  private val map: rng.Rand[Double] = rng.doubleViaMap
  println(rng.unit(1)(rng)._1)
  println(map(rng)._1)

  /**
   * 练习6.7
   */
  println(rng.intsViaSequence(3)(rng))
}
