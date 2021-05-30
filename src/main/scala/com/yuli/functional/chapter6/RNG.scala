package com.yuli.functional.chapter6

/**
 * 随机数生成器接口
 */
trait RNG {
  def nextInt: (Int, RNG)

  /**
   * 练习6.1
   *
   * @param rng
   * @return
   */
  def nonNegativeInt(rng: RNG): (Int, RNG)

  /**
   * 练习6.2
   * 返回一个0~1之间的double数
   *
   * @param rng
   * @return
   */
  def double(rng: RNG): (Double, RNG)

  /**
   * 练习6.3
   *
   * @param rng
   * @return
   */
  def intDouble(rng: RNG): ((Int, Double), RNG)

  def doubleInt(rng: RNG): ((Double, Int), RNG)

  def double3(rng: RNG): ((Double, Double, Double), RNG)

  /**
   * 练习6.4
   *
   * @param count
   * @param rng
   * @return
   */
  def ints(count: Int)(rng: RNG): (List[Int], RNG)

  /**
   * 将rng抽象为类型别名
   *
   * @tparam A
   */
  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  /**
   * 将一个常量转化为组合子
   *
   * @param a
   * @tparam A
   * @return
   */
  def unit[A](a: A): Rand[A] = rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] = rng => {
    val (a, rng2) = s(rng)
    (f(a), rng2)
  }

  /**
   * 生成一哥大于0能被2整除的数
   * @return
   */
  def nonNegativeEven: Rand[Int] = map(nonNegativeInt)(i => i - 1 % 2)

  /**
   * 练习6.5
   * 用map生成一个double
   * @return
   */
  def doubleViaMap: Rand[Double] = map(nonNegativeInt)(i => i / (Int.MaxValue.toDouble + 1))

  /**
   * 练习6.6
   *
   * @param ra
   * @param rb
   * @param f
   * @tparam A
   * @tparam B
   * @tparam C
   * @return
   */
  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = rng => {
    val (a, rng2) = ra(rng)
    val (b, rng3) = rb(rng2)
    (f(a, b), rng3)
  }

  /**
   * 对组合行为进行抽象
   *
   * @param ra
   * @param rb
   * @tparam A
   * @tparam B
   * @return
   */
  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] = map2(ra, rb)((_, _))

  val randIntDouble: Rand[(Int, Double)] = both(int, double)
  val randDoubleInt: Rand[(Double, Int)] = both(double, int)

  /**
   * 练习6.7
   *
   * @param fs
   * @tparam A
   * @return
   */
  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = fs.foldRight(unit(Nil: List[A]))((f, acc) => map2(f, acc)(_ :: _))

  def intsViaSequence(count: Int): Rand[List[Int]] = sequence(List.fill(count)(int))

  /**
   * 生成0到n之间一个整数
   * @param n
   * @return
   */
  def nonNegativeLessThan(n: Int): Rand[Int] = { rng =>
    val (i, rng2) = nonNegativeInt(rng)
    val mod = i % n
    if (i + (n - 1) - mod >= 0)
      (mod, rng2)
    else nonNegativeLessThan(n)(rng)
  }

  /**
   * 练习6.8
   * 实现flatmap然后实现nonNegativeLessThan
   *
   * @param f
   * @param g
   * @tparam A
   * @tparam B
   * @return
   */
  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = rng => {
    val (a, rng2) = f(rng)
    g(a)(rng2)
  }

  def nonNegativeLessThan2(n: Int): Rand[Int] = {
    flatMap(nonNegativeInt) { i =>
      val mod = i % n
      if (i + (n - 1) - mod >= 0) unit(mod) else nonNegativeLessThan(n)
    }
  }

  /**
   * 练习6.9
   * 用flatmao实现map和flatmap
   *
   * @param s
   * @param f
   * @tparam A
   * @tparam B
   * @return
   */
  def mapViaFlatMap[A, B](s: Rand[A])(f: A => B): Rand[B] = flatMap(s)(i => unit(f(i)))

  def map2ViaFlatMap[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = flatMap(ra)(a => mapViaFlatMap(rb)(b => f(a, b)))

  /**
   * 投骰子
   * @return
   */
  def rollDie:Rand[Int] = map(nonNegativeLessThan(6))(_+1)
}
