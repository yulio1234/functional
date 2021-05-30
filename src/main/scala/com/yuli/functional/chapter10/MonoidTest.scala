package com.yuli.functional.chapter10

object MonoidTest extends App {
  private val words = List("Hic", "est", "Index")
  println(words)
  private val str: String = words.foldLeft(Monoid.stringMonoid.zero)(Monoid.stringMonoid.op)
  private val str1: String = words.foldRight(Monoid.stringMonoid.zero)(Monoid.stringMonoid.op)
  println(str)
  println(str1)

}
