package com.yuli.functional.chapter3

object TreeTest extends App {
  //练习3.25
  private val tree: Branch[String] = Branch(Branch(Leaf("a"), Leaf("b")), Branch(Leaf("c"), Leaf("d")))
  println(Tree.size(tree))
  //练习3.26
  private val maxValue: Branch[Int] = Branch(Branch(Leaf(1), Leaf(3)), Branch(Leaf(11), Leaf(100)))
  println(Tree.maximum(maxValue))
  //练习3.27
  println(Tree.depth(tree))
  //练习3.28
  println(Tree.map(maxValue)(_ + 1))
  //练习3.29
  println(Tree.foldSize(tree))
  println(Tree.foldMaximum(maxValue))
  println(Tree.foldDepth(tree))
  println(Tree.foldMap(maxValue)(_ + 1))
}
