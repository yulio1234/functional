package com.yuli.functional.chapter8

import com.yuli.functional.chapter6.{RNG, State}
import com.yuli.functional.chapter8.Prop.{FailedCase, SuccessCount}

trait Prop {
  def check: Either[(FailedCase,SuccessCount),SuccessCount]

  def &&(p: Prop): Prop = ???

}
object Prop{
  type FailedCase = String
  type SuccessCount = Int
}

