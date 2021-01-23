package com.yuli.functional.chapter5

/**
 * 描述：
 *
 * @author yuli
 * @date 2021/1/12
 */
object StreamTest extends App {
  private val stream: Stream[Int] = Stream(1, 2, 3, 4, 5)
//  /**
//   * 练习5.1
//   */
//  println(stream.toList)
//  /**
//   * 练习5.2
//   */
//  println(stream.take(2).toList)
//  println(stream.drop(3).toList)
//  /**
//   * 练习5.3
//   */
//  println(stream.takeWhile(p => p % 2 == 0).toList)
//  /**
//   * 练习5.4
//   */
//  println(stream.forAll(p => p.isFinite))
//  /**
//   * 练习5.5
//   */
//  println(stream.takeWhileFold(p => p % 2 == 0).toList)
//  /**
//   * 练习5.6
//   */
//  println(stream.headOptionFold)
//  /**
//   * 练习5.7
//   */
//  println(stream.append(Stream.apply(6)).toList)
//  println(stream.map(_ + 1).toList)
//
//  stream.map(_ + 10).filter(_ % 2 == 0).toList
//  /**
//   * 无限流，使用惰性求值可以避免中间运算,就不会产生死循环
//   */
//  val ones: Stream[Int] = Stream.cons(1, ones);
//  println(ones.take(5).toList)
//  println(ones.exists(_ % 2 != 0))
//  println(ones.map(_ + 1).exists(_ % 2 == 0))
//  println(Stream.from(1).take(5).toList)
//  println(Stream.fibs.take(8).toList)
//  println(Stream.fibsUnfold.take(8).toList)
//  println(Stream.constantUnfold(1).take(5).toList)
//  println(Stream.from(1).take(5).toList)
//
//  /**
//   * 练习5.13
//   */
//  println(stream.mapViaUnfold(_ + 1).toList)
//  println(stream.takeViaUnfold(3).toList)
//  println(stream.takeWhileViaUnfold(_ % 2 == 0).toList)
//
//  private val stream2: Stream[Int] = Stream(1, 2, 3,5)
//  println(stream.startsWith(stream2))
  /**
   * 练习5.15
   */
  stream.tails.toList.foreach(f => println(f.toList))
}
