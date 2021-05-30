package com.yuli.functional.chapter6

import com.yuli.functional.chapter6.RNGTest.rng

object RNGTest extends App {
  private val rng: SimpleRNG = SimpleRNG(42)
  private val nextRNG: (Int, RNG) = rng.nextInt
  /**
   * 联系6.1
   */
  private val tuple: (Int, RNG) = nextRNG._2.nonNegativeInt(nextRNG._2)
  println(tuple._1)
  private val nextInt: (Int, RNG) = nextRNG._2.nextInt
  println(nextInt._1)
  println(rng.unit(1)(rng)._1)

  println(rng.map(rng.unit(1))(_+1)(rng)._1)
  private val even: rng.Rand[Int] = rng.nonNegativeEven
  println(even(rng))
  private val die: rng.Rand[Int] = rng.rollDie
  println(die(rng))

//  /**
//   * 练习6.7
//   */
//  println(rng.intsViaSequence(3)(rng))
}
