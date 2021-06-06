package com.yuli.functional.chapter6

object RNGTest extends App {
  private val rng: RNG.SimpleRNG = RNG.SimpleRNG(42)
  private val nextRNG: (Int, RNG) = rng.nextInt
  /**
   * 联系6.1
   */
  private val tuple: (Int, RNG) = RNG.nonNegativeInt(nextRNG._2)
  println(tuple._1)
  private val nextInt: (Int, RNG) = nextRNG._2.nextInt
  println(nextInt._1)
  println(RNG.unit(1)(rng)._1)

  println(RNG.map(RNG.unit(1))(_ + 1)(rng)._1)
  private val even: RNG.Rand[Int] = RNG.nonNegativeEven
  println(even(rng))
  private val die: RNG.Rand[Int] = RNG.rollDie
  println(die(rng))

  //  /**
  //   * 练习6.7
  //   */
  //  println(rng.intsViaSequence(3)(rng))
}
