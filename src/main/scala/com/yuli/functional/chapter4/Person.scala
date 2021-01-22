package com.yuli.functional.chapter4

case class Person()
object Person{
}
sealed class Name(val value: String)

sealed class Age(val value: Int)