package com.yuli.functional.chapter3

/**
 *
 *
 * @author yuli
 * @date 2020/7/28
 */
object TestApp extends App {
  val list: List[Int] = List(1, 2, 3, 4, 5)
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

  private val list9: List[Int] = List.copy(List(1, 2, 3, 4))
  println("练习3.8" + list9)

  private val i: Int = List.lengthFoldLeft(list9)
  println(i)
  println(List.sumFoldLeft(list))
  println(List.sumFoldRight(list))


  //3.10测试尾递归fold
  private val value: Int = List.foldLeft(List(1, 2, 3, 4), 0)(_ + _)
  println("3.10测试：" + value)

  private val flipList = List(1, 2, 3, 4, 5, 6, 7, 8, 9)
  //3.12集合翻转
  private val flipValue: List[Int] = List.flip(flipList)
  println(flipList)
  println("3.12测试：" + flipValue)

  //练习3.13
  List.foldRightByLeft(List(1, 2, 3, 4), Nil:List[Int])((a,b)=>{println(a);Cons(a,b)})
  List.foldRight(List(1, 2, 3, 4), Nil:List[Int])((a,b)=>{println(a);Cons(a,b)})

  //练习3.14
  private val value1: List[Int] = List.appendFold(List(1, 2, 3, 4), List(5, 6, 7, 8))
  println("练习3.14"+value1)

  //练习3.15
  private val foldList1: List[Int] = List(1, 2, 3, 4)
  private val foldList2: List[Int] = List(5, 6, 7, 8)
  private val foldList3: List[Int] = List(9, 10, 11, 12)
  private val lists: List[List[Int]] = List(foldList1, foldList2, foldList3)
  private val listsValue: List[Int] = List.mergeLists(lists)
  println(listsValue)

  //练习3.16
  println(List.plusOne(foldList1))
  //练习3.17
  private val doubles: List[Double] = List(1.1, 1.2, 1.3)
  private val strings: List[String] = List.convertString(doubles)
  println(strings)
  //练习3.18
  println(List.map(doubles)(_ + 1))
  //练习3.19
  private val filterResult: List[Int] = List.filter(List(1, 2, 3, 4, 5, 6, 7, 8, 9))(a => a % 2 != 1)
  println(filterResult)
  //练习3.20
  println(List.flatMap(List(1, 2, 3))(i => List(i, i)))
  //练习3.22
  println(List.listPlus(List(1, 2, 3), List(4, 5, 6)))
}
