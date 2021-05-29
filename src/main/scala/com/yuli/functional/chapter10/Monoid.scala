package com.yuli.functional.chapter10

trait Monoid[A] {
  def op(al: A, a2: A)
}
