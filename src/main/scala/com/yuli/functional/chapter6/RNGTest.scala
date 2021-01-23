package com.yuli.functional.chapter6

object RNGTest extends App {
  private val rng: SimpleRNG = SimpleRNG(42)
  private val nextRNG: (Int, RNG) = rng.nextInt
  println(nextRNG._1)
  private val nextInt: (Int, RNG) = nextRNG._2.nextInt
  println(nextInt._1)

}
