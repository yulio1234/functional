package com.yuli.functional.chapter6

trait RngState {

  def nextInt: Rand[Int]

  /**
   * 练习6.1
   *
   * @param rng
   * @return
   */
  def nonNegativeInt: Rand[Int]

  /**
   * 练习6.2
   * 返回一个0~1之间的double数
   *
   * @param rng
   * @return
   */
  def double: Rand[Double]

  /**
   * 练习6.3
   *
   * @param rng
   * @return
   */
  def intDouble: Rand[(Int, Double)]

  def doubleInt: Rand[(Double, Int)]

  def double3: Rand[(Double, Double, Double)]

  /**
   * 练习6.4
   *
   * @param count
   * @param rng
   * @return
   */
  def ints(count: Int): Rand[List[Int]]

  /**
   * 将rng抽象为类型别名
   *
   * @tparam A
   */
  type Rand[+A] = State[RNG, A]

  val int: Rand[Int] = this.nextInt
  val ns: Rand[List[Int]] = int.flatMap(x =>
    int.flatMap(y =>
      ints(x).map(xs => xs.map(_ % y))
    )
  )

  val ns2: Rand[List[Int]] = for {
    x <- nextInt
    y <- nextInt
    xs <- ints(x)
  } yield xs.map(_ % y)
}
