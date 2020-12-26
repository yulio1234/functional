package com.yuli.functional.chapter4


object OptionTest extends App {
  private val value = Seq(1.0, 2, 3, 4)
  println(Demo.mean(value))
  println(Demo.variance(value))
  //练习4.3
  private val list: List[Option[Int]] = Some(1) :: Some(2) :: Some(3)::None :: Nil
  println(Demo.sequence(list))
}
