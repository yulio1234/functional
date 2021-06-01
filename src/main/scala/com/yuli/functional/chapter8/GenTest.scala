package com.yuli.functional.chapter8

import com.yuli.functional.chapter6.{RNG, SimpleRNG}

object GenTest extends App {
  private val value: Gen[Int] = Gen.choose(1, 5)
  println(value)

}
