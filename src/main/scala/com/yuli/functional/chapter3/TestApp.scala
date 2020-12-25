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

  private val i: Int = List.lengthFoldLeft(list9)
  println(i)
  println(List.sumFoldLeft(list))
  println(List.sumFoldRight(list))


  //3.10测试尾递归fold
  private val value: Int = List.foldLeft(List(1, 2, 3, 4), 0)(_+_)
  println("3.10测试："+value)

  //3.12集合翻转
  private val flipValue: List[Int] = List.flip(List(1, 2, 3, 4))
  println(List(1, 2, 3, 4))
  println("1.11测试：" + flipValue)

  //练习3.13
  private val result: Int = List.foldRightByLeft(List(1, 2, 3, 4), 0)(_ + _)
  println(result)
  //练习3.14
  private val value1: List[Int] = List.appendFold(List(1, 2, 3, 4), List(5, 6, 7, 8))
  println(value1)

  //练习3.15
  private val foldList1: List[Int] = List(1, 2, 3, 4)
  private val foldList2: List[Int] = List(5, 6, 7, 8)
  private val foldList3: List[Int] = List(9, 10, 11, 12)
  private val lists: List[List[Int]] = List(foldList1, foldList2, foldList3)
  private val listsValue: List[Int] = List.mergeLists(lists)
  println(listsValue)
}
