package com.yuli.functional.chapter6

case class SimpleRNG(seed: Long) extends RNG {
  override def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }

  /**
   * 练习6.1
   *
   * @param rng
   * @return
   */
  override def nonNegativeInt(rng: RNG): (Int, RNG) = {

    val (i, r) = rng.nextInt
    (if (i < 0) -(i + 1) else i, r)
  }
}
