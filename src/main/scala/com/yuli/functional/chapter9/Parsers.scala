package com.yuli.functional.chapter9

import scala.util.matching.Regex


trait Parsers[Parser[+_]] {
  self =>
  def run[A](p: Parser[A])(input: String): Either[ParserError, A]

//  type Parser[+A] = String => Either[ParserError, A]

  implicit def string(s: String): Parser[String]

  /**
   * 将解析器转换为解析操作器
   *
   * @param p
   * @tparam A
   * @return
   */
  implicit def operators[A](p: Parser[A]): ParserOps[A] = ParserOps[A](p)

  implicit def asStringParSer[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] = ParserOps(f(a))

  implicit def regex(r: Regex): Parser[String]

  /**
   * 识别一个字符
   *
   * @param c
   * @return
   */
  def char(c: Char): Parser[Char] = string(c.toString).map(_.charAt(0))

  def succeed[A](a: A): Parser[A] = string("") map (_ => a)

  def orString(s1: String, s2: String): Parser[String]

  def or[A](p1: Parser[A], p2: => Parser[A]): Parser[A]

  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] = if (n <= 0) succeed(List()) else map2(p, listOfN(n - 1, p))(_ :: _)

  /**
   * 返回匹配集合,因为p2是惰性求值，所以第一个参数失败就会返回
   *
   * @param p
   * @tparam A
   * @return
   */
  def many[A](p: Parser[A]): Parser[List[A]] = map2(p, many(p))(_ :: _) or succeed(List())

  /**
   * 通过flatMap得到a,然后f andThen succeed得到转换函数，传递给flatMap
   *
   * @param a
   * @param f
   * @tparam A
   * @tparam B
   * @return
   */
  def map[A, B](a: Parser[A])(f: A => B): Parser[B] = flatMap(a)(f andThen succeed)

  def slice[A](p: Parser[A]): Parser[String]

  /**
   *
   * 执行完一个分析器后执行另外一个
   * p2必须惰性求值，不然会死循环
   *
   * @param p
   * @param p2
   * @tparam A
   * @tparam B
   * @return
   */
  def product[A, B](p: Parser[A], p2: => Parser[B]): Parser[(A, B)] = flatMap(p)(a => map(p2)(b => (a, b)))

  def flatMap[A, B](p: Parser[A])(f: A => Parser[B]): Parser[B]

  /**
   * p2必须惰性求值，不然会死循环
   *
   * @param p
   * @param p2
   * @param f
   * @tparam A
   * @tparam B
   * @tparam C
   * @return
   */
  def map2[A, B, C](p: Parser[A], p2: => Parser[B])(f: (A, B) => C): Parser[C] = for {a <- p; b <- p2} yield f(a, b)

  def many1[A](p: Parser[A]): Parser[List[A]] = map2(p, many(p))(_ :: _)

  /**
   * 当失败时，将message作为错误消息
   *
   * @param message
   * @param p
   * @tparam A
   * @return
   */
  def label[A](message: String)(p: Parser[A]): Parser[A]

  /**
   * 当失败时，将e添加到由p返回当错误栈上
   *
   * @param message
   * @param p
   * @tparam A
   * @return
   */
  def scope[A](message: String)(p: Parser[A]): Parser[A]

  def attempt[A](p: Parser[A]): Parser[A]


  case class ParserOps[A](p: Parser[A]) {
    def |[B >: A](p2: Parser[B]): Parser[B] = self.or(p, p2)

    def or[B >: A](p2: => Parser[B]): Parser[B] = self.or(p, p2)

    def map[B](f: A => B): Parser[B] = self.map(p)(f)

    def slice[B]: Parser[String] = self.slice(p)

    def product[B](p2: Parser[B]): Parser[(A, B)] = self.product(p, p2)

    def **[B](p2: Parser[B]): Parser[(A, B)] = self.product(p, p2)

    def flatMap[B](f: A => Parser[B]): Parser[B] = self.flatMap(p)(f)
  }
}

case class ParserError(stack: List[(Location, String)])

case class Location(input: String, offset: Int = 0) {
  lazy val line = input.slice(0, offset + 1).count(_ == '\n') + 1
  lazy val col = input.slice(0, offset + 1).lastIndexOf('\n') match {
    case -1 => offset + 1
    case lineStart => offset - lineStart
  }

  def toError(message: String): ParserError = ParserError(List((this, message)))
}
