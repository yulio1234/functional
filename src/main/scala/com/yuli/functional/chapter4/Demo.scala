package com.yuli.functional.chapter4

/**
 * 描述：
 *
 * @author yuli
 * @date 2020/12/24
 */
class Demo {
  /**
   * 使用some来定义mean，可以避免抛出异常
   *
   * @param xs
   * @return
   */
  def mean(xs: Seq[Double]): Option[Double] = {
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)
  }

//  def variance(xs: Seq[Double]): Option[Double] = {
//    val m = mean(xs)
//
//  }
}
