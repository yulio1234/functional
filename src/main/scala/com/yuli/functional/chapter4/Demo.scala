package com.yuli.functional.chapter4

/**
 * 描述：
 *
 * @author yuli
 * @date 2020/12/24
 */
object Demo {
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

  /**
   * 练习4.2
   * 用flatMap实现方差
   *
   * @param xs
   * @return
   */
  def variance(xs: Seq[Double]): Option[Double] = mean(xs).flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))

  /**
   * 提升函数，将普通函数转换成在一个Option值的上下文里进行操作的函数
   *
   * @param f
   * @tparam A
   * @tparam B
   * @return
   */
  def lift[A, B](f: A => B): Option[A] => Option[B] = _ map f

  /**
   * 将绝对值计算提升为option
   */
  val absOption: Option[Double] => Option[Double] = lift(math.abs)

  /**
   * 根据两个因子计算保险费
   *
   * @param age
   * @param numberOfSpeedingTickets
   * @return
   */
  def insuranceRateQuote(age: Int, numberOfSpeedingTickets: Int): Double = (numberOfSpeedingTickets * age).toDouble

  /**
   * 接受一个非严格求值的A参数，在对a求值时捕获异常转换为None
   *
   * @param a
   * @tparam A
   * @return
   */
  def Try[A](a: => A): Option[A] = try Some(a) catch {
    case e: Exception => None
  }

  def parseInsuranceRateQuote(age: String, numberOfSpeedingTickets: String): Option[Double] = {
    val optAge: Option[Int] = Try(age.toInt)
    val optTickets = Try(numberOfSpeedingTickets.toInt)
    //如果任何一个解析失败，将返回None
    map2(optAge, optTickets)(insuranceRateQuote)
  }

  /**
   * 练习4.3使用二元函数来组合两个Option的值
   *
   * @param a
   * @param b
   * @param f
   * @tparam A
   * @tparam B
   * @tparam C
   * @return
   */
  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = (a, b) match {
    case (Some(a), Some(b)) => Some(f(a, b))
    case (_,_) => None
  }

  /**
   * 练习4.4
   * 使用提升函数，遇到None直接返回None
   *
   * @param a
   * @tparam A
   * @return
   */
  def sequence[A](a: List[Option[A]]): Option[List[A]] = a.foldRight(Some(Nil): Option[List[A]])((a, b) => map2(a, b)((a, b) => a :: b))

  /**
   * 练习4.5
   * 将map和sequence融合，实现更好的性能
   * @param a
   * @param f
   * @tparam A
   * @tparam B
   * @return
   */
  def traverse[A,B](a:List[A])(f: A=>Option[B]):Option[List[B]] = a.foldRight(Some(Nil):Option[List[B]])((a,b)=>map2(f(a),b)(_::_))
}
