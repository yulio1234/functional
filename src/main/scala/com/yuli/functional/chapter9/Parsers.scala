package com.yuli.functional.chapter9


trait Parsers[ParserError, Parser[+_]] {
  self =>
  def run[A](p: Parser[A])(input: String): Either[ParserError, A]

  def char(c: Char): Parser[Char]

  implicit def string(s: String): Parser[String]

  implicit def operators[A](p: Parser[A]): ParserOps[A] = ParserOps[A](p)

  implicit def asStringParSer[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] = ParserOps(f(a))

  def orString(s1: String, s2: String): Parser[String]

  def or[A](p1: Parser[A], p2: Parser[A]): Parser[A]

  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]]

  def many[A](p: Parser[A]): Parser[List[A]]

  def map[A, B](a: Parser[A])(f: A => B): Parser[B]

  case class ParserOps[A](p: Parser[A]) {
    def |[B >: A](p2: Parser[B]): Parser[B] = self.or(p, p2)

    def or[B >: A](p2: => Parser[B]): Parser[B] = self.or(p, p2)
  }
}