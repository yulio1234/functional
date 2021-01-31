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

  /**
   * 练习6.2
   * 返回一个0~1之间的double数
   *
   * @param rng
   * @return
   */
  override def double(rng: RNG): (Double, RNG) = {
    val (i, r) = nonNegativeInt(rng)
    (i / (Int.MaxValue.toDouble + 1), r)
  }

  /**
   * 练习6.3
   *
   * @param rng
   * @return
   */
  override def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, r1) = rng.nextInt
    val (d, r2) = double(r1)
    ((i, d), r2)
  }

  override def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val (i, r1) = rng.nextInt
    val (d, r2) = double(r1)
    ((d, i), r2)

  }

  override def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, r1) = double(rng)
    val (d2, r2) = double(r1)
    val (d3, r3) = double(r2)
    ((d1, d2, d3), r3)
  }

  /**
   * 练习6.4
   * 生成一组随机数
   * @param count
   * @param rng
   * @return
   */
  override def ints(count: Int)(rng: RNG): (List[Int], RNG) = count match {
    case i if i > 0 =>
      val (i, r) = rng.nextInt
      val (list, r2) = ints(i - 1)(r)
      (i :: list, r2)
    case i if i == 0 =>
      (Nil, rng)
  }

}
