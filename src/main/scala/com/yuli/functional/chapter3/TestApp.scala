package com.yuli.functional.chapter3

/**
 *
 *
 * @author yuli
 * @date 2020/7/28
 */
object TestApp extends App {
  val list : List[Int] = List(1,2,3,4,5)
  private val list2: List[Int] = List.tail(list)
  println(list2)
  private val list3: Cons[Int] = List.setHead(1, list2)
  println(list3)

  private val list4: List[Int] = List.drop(2, list3)
  println(list4)


  private val list5: List[Int] = List(1, 1, 1, 3, 4, 1, 5, 1, 6)
  private val list6: List[Int] = List.dropWhile(list5)(x => x == 1)
  println(list6)

  private val list7: List[List[Int]] = List(list5, list)
  println(list7)
  //删除最后一个
  private val list8: List[Int] = List.init(list)
  println(list8)

  private val list9: List[Int] = List.foldRight(List(1, 2, 3, 4), Nil: List[Int])(Cons(_, _))
  println(list9)

  private val i: Int = List.length(list9)
  println(i)

  println(List.sum2(list))
  println(List.sum3(list))




}
