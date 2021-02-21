package com.yuli.functional.chapter8

import com.yuli.functional.chapter6.RNG

object GenTest extends App {
  private val run: RNG => (Int, RNG) = Gen.choose(1, 5).simple.run
  println(run)

}
