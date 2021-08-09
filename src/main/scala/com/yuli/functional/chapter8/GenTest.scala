package com.yuli.functional.chapter8

import com.yuli.functional.chapter6.RNG
import com.yuli.functional.chapter7.Par
import com.yuli.functional.chapter8.Gen.{choose, listOf, unit, weighted}
import com.yuli.functional.chapter8.Prop.forAll

import java.util.concurrent.{ExecutorService, Executors}

object GenTest extends App {
  private val smallInt: Gen[Int] = Gen.choose(-10, 10)
  private val as: Gen[List[Int]] = smallInt.listOfN(2)
  private val rng: RNG.SimpleRNG = RNG.SimpleRNG(System.currentTimeMillis())
  Prop.randomStream(as)(rng).zip(LazyList.from(0)).take(10).foreach(println)
  private val prop: Prop = forAll(listOf(smallInt)) { ns =>
    val max = ns.max
    !ns.exists(_ > max)
  }
  Prop.run(prop)
  private val es: ExecutorService = Executors.newCachedThreadPool()
  private val prop1: Prop = Prop.check {
    val p = Par.map(Par.unit(1))(_ + 1)
    val p2 = Par.unit(2)
    p(es).get() == p2(es).get()
  }
  prop && prop1
  Prop.run(prop1)

  private val s: Gen[ExecutorService] = weighted(choose(1, 4).map(Executors.newFixedThreadPool) -> .75, unit(Executors.newCachedThreadPool) -> .25)


}
