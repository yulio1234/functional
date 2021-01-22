package com.yuli.functional.chapter5

/**
 * 描述：
 *
 * @author yuli
 * @date 2021/1/12
 */
object StreamTest extends App {
  private val stream: Stream[Int] = Stream(1, 2, 3, 4, 5)
  /**
   * 练习5.1
   */
  println(stream.toList)
  /**
   * 练习5.2
   */
  println(stream.take(2).toList)
  println(stream.drop(3).toList)
  /**
   * 练习5.3
   */
  println(stream.takeWhile(p => p % 2 == 0).toList)
  /**
   * 练习5.4
   */
  println(stream.forAll(p => p.isFinite))
}
