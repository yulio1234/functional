package com.yuli.functional.chapter6

/**
 * 随机数生成器接口
 */
trait RNG {
  def nextInt: (Int, RNG)

  /**
   * 练习6.1
   * @param rng
   * @return
   */
  def nonNegativeInt(rng:RNG):(Int,RNG)
}
