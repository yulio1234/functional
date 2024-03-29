package com.yuli.functional.chapter10

import com.yuli.functional.chapter3.{Branch, Leaf, Tree}
import com.yuli.functional.chapter7.Nonblocking.Par
import com.yuli.functional.chapter7.Nonblocking.Par.toParOps

/**
 * 幺半群
 * 满足结合律(associativity)和同一律(identity)
 * 一个类型A
 * 结合律：op(op(x,y),z) == op(x,op(y,z))
 * 同一律：单位元zero:A op(x,zero) == x || op(zero,x) == x
 *
 * @tparam A
 */
trait Monoid[A] {
  def op(a1: A, a2: A): A //满足op(op(x,y),z) == op(x,op(y,z))

  def zero: A //满足op(x,zero) = x和op(zero,x)==x
}

object Monoid {
  val stringMonoid = new Monoid[String] {
    override def op(a1: String, a2: String): String = a1 + a2

    override def zero: String = ""
  }

  def listMonoid[A] = new Monoid[List[A]] {
    override def op(a1: List[A], a2: List[A]): List[A] = a1 ::: a2

    override def zero: List[A] = Nil
  }

  /**
   * 联系10.1
   */
  val intAddition: Monoid[Int] = new Monoid[Int] {
    override def op(a1: Int, a2: Int): Int = a1 + a2

    override def zero: Int = 0
  }
  val intMultiplication: Monoid[Int] = new Monoid[Int] {
    override def op(a1: Int, a2: Int): Int = a1 * a2

    override def zero: Int = 1
  }
  val booleanOr: Monoid[Boolean] = new Monoid[Boolean] {
    override def op(a1: Boolean, a2: Boolean): Boolean = a1 || a2

    override def zero: Boolean = false
  }
  val booleanAnd: Monoid[Boolean] = new Monoid[Boolean] {
    override def op(a1: Boolean, a2: Boolean): Boolean = a1 && a2

    override def zero: Boolean = true
  }

  def optionMonoid[A]: Monoid[Option[A]] = new Monoid[Option[A]] {
    override def op(a1: Option[A], a2: Option[A]): Option[A] = a1 orElse a2

    override def zero: Option[A] = None
  }

  /**
   * 反转op的两个参数
   *
   * @param m
   * @tparam A
   * @return
   */
  def dual[A](m: Monoid[A]): Monoid[A] = new Monoid[A] {
    override def op(a1: A, a2: A): A = m.op(a2, a1)

    override def zero: A = m.zero
  }

  def endoMonoid[A]: Monoid[A => A] = new Monoid[A => A] {
    override def op(a1: A => A, a2: A => A): A => A = a1 compose a2

    override def zero: A => A = (a: A) => a
  }

  def concatenate[A](as: List[A], m: Monoid[A]): A = as.foldLeft(m.zero)(m.op)

  /**
   * 练习10.5
   *
   * @param as
   * @param m
   * @param f
   * @tparam A
   * @tparam B
   * @return
   */
  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B = as.foldLeft(m.zero)((b, a) => m.op(b, f(a)))

  /**
   * 练习10.6
   *
   * @param as
   * @param z
   * @param f
   * @tparam A
   * @tparam B
   * @return
   */
  def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B = foldMap(as, endoMonoid[B])(f.curried)(z)

  def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B = foldMap(as, dual(endoMonoid[B]))(a => b => f(b, a))(z)

  /**
   * 练习10.7
   *
   * @param v
   * @param m
   * @param f
   * @tparam A
   * @tparam B
   * @return
   */
  def foldMapV[A, B](as: IndexedSeq[A], m: Monoid[B])(f: A => B): B =
    if (as.length == 0) {
      m.zero
    } else if (as.length == 1) {
      f(as(0))
    } else {
      val (l, r) = as.splitAt(as.length / 2)
      m.op(foldMapV(l, m)(f), foldMapV(r, m)(f))
    }

  /**
   * 练习10.9
   *
   * @param ints
   * @return
   */
  def ordered(ints: IndexedSeq[Int]): Boolean = {
    val monoid = new Monoid[Option[(Int, Int, Boolean)]] {
      override def op(o1: Option[(Int, Int, Boolean)], o2: Option[(Int, Int, Boolean)]): Option[(Int, Int, Boolean)] = (o1, o2) match {
        case (Some((x1, y1, p)), Some((x2, y2, q))) => Some((x1 min x2, y1 max y2, p && q && y1 <= x2))
        case (x, None) => x
        case (None, x) => x
      }

      override def zero: Option[(Int, Int, Boolean)] = None
    }
    foldMapV(ints, monoid)(i => Some(i, i, true)).map(_._3).getOrElse(true)
  }

  def par[A](m: Monoid[A]): Monoid[Par[A]] = new Monoid[Par[A]] {
    override def op(a1: Par[A], a2: Par[A]): Par[A] = a1.map2(a2)(m.op)

    override def zero: Par[A] = Par.unit(m.zero)
  }

  def parFoldMap[A, B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): Par[B] = Par.parMap(v)(f).flatMap { bs =>
    foldMapV(bs, par(m))(b => Par.lazyUnit(b))
  }

  /**
   * 单词接口
   */
  sealed trait WC

  /**
   * 存储不完整单词
   *
   * @param chars
   */
  case class Stub(chars: String) extends WC

  /**
   * 存储不完整的单词和完整单词的个数
   *
   * @param lStub
   * @param words
   * @param rStub
   */
  case class Part(lStub: String, words: Int, rStub: String) extends WC

  /**
   * word count 幺半群
   */
  val wcMonoid: Monoid[WC] = new Monoid[WC] {
    override def op(a: WC, b: WC): WC = (a, b) match {
      case (Stub(c), Stub(d)) => Stub(c + d) //如果是两个部分单词，直接组合
      case (Stub(c), Part(l, w, r)) => Part(c + l, w, r) //左边部分组合
      case (Part(l, w, r), Stub(c)) => Part(l, w, r + c) //右边比分组合
      case (Part(l1, w1, r1), Part(l2, w2, r2)) => Part(l1, w1 + (if ((r1 + l2).isEmpty) 0 else 1) + w2, r2) //中间部分组合，如果不为空，就说明是完整单词，就将单词数加一
    }

    override def zero: WC = Stub("") //默认值""
  }

  /**
   * 计算句子单词数
   *
   * @param sentence
   * @return
   */
  def count(sentence: String): Int = {
    //如果是空格，就返回一个部分
    def wc(c: Char): WC = if (c.isWhitespace) Part("", 0, "") else Stub(c.toString)

    /**
     * 得到单个单词的单词数
     *
     * @param word
     * @return
     */
    def unStub(word: String): Int = word.length min 1

    /**
     * 使用foldMapV结合字符集合成为一个幺半群
     */
    foldMapV(sentence.toIndexedSeq, wcMonoid)(wc) match {
      case Stub(word) => unStub(word)
      case Part(l, w, r) => unStub(l) + w + unStub(r)
    }

  }

  /**
   * 练习10.16
   *
   * @param A
   * @param B
   * @tparam A
   * @tparam B
   * @return
   */
  def productMonoid[A, B](A: Monoid[A], B: Monoid[B]): Monoid[(A, B)] = new Monoid[(A, B)] {
    override def op(x: (A, B), y: (A, B)): (A, B) = (A.op(x._1, y._1), B.op(x._2, y._2))

    override def zero: (A, B) = (A.zero, B.zero)
  }

  /**
   * 抽象出一个通用的折叠方法，可以处理任何包含fold方法的对象
   *
   * @tparam F
   */
  trait Foldable[F[_]] {
    def foldRight[A, B](as: F[A])(z: B)(f: (A, B) => B): B

    def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) => B): B

    def foldMap[A, B](as: F[A])(f: A => B)(mb: Monoid[B]): B

    def concatenate[A](as: F[A])(m: Monoid[A]): A = foldLeft(as)(m.zero)(m.op)

    /**
     * 练习10.15 将Foldable结构转换为List
     * @param as
     * @tparam A
     * @return
     */
    def toList[A](as: F[A]): List[A] = foldRight(as)(List[A]())(_::_)
  }

  /**
   * 练习10.12 实现List，IndexSeq和Stream
   */
  object ListFoldable extends Foldable[List] {
    override def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B = as.foldRight(z)(f)

    override def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B = as.foldLeft(z)(f)

    override def foldMap[A, B](as: List[A])(f: A => B)(mb: Monoid[B]): B = as.foldLeft(mb.zero)((b, a) => mb.op(b, f(a)))
  }

  object IndexSeqFoldable extends Foldable[IndexedSeq] {
    override def foldRight[A, B](as: IndexedSeq[A])(z: B)(f: (A, B) => B): B = as.foldRight(z)(f)

    override def foldLeft[A, B](as: IndexedSeq[A])(z: B)(f: (B, A) => B): B = as.foldLeft(z)(f)

    override def foldMap[A, B](as: IndexedSeq[A])(f: A => B)(mb: Monoid[B]): B = foldMapV(as, mb)(f)
  }

  object StreamFoldable extends Foldable[LazyList] {
    override def foldRight[A, B](as: LazyList[A])(z: B)(f: (A, B) => B): B = as.foldRight(z)(f)

    override def foldLeft[A, B](as: LazyList[A])(z: B)(f: (B, A) => B): B = as.foldLeft(z)(f)

    override def foldMap[A, B](as: LazyList[A])(f: A => B)(mb: Monoid[B]): B = as.foldLeft(mb.zero)((b, a) => mb.op(b, f(a)))
  }

  /**
   * 练习10.13实现二叉树的Foldable
   */
  object TreeFoldable extends Foldable[Tree] {
    override def foldRight[A, B](as: Tree[A])(z: B)(f: (A, B) => B): B = as match {
      case Leaf(a) => f(a, z) //如果是叶子节点就执行f函数
      case Branch(left, right) => foldRight(left)(foldRight(right)(z)(f))(f) //找到最左叶子节点得到转换后的值，然后开始找右节点的可转换值
    }

    override def foldLeft[A, B](as: Tree[A])(z: B)(f: (B, A) => B): B = as match {
      case Leaf(a) => f(z, a)
      case Branch(left, right) => foldLeft(right)(foldLeft(left)(z)(f))(f) //先找到最右叶子结点得到转换后的值，然后开始找左节点可以转换的值
    }

    override def foldMap[A, B](as: Tree[A])(f: A => B)(mb: Monoid[B]): B = as match {
      case Leaf(a) => f(a) //当是叶子节点的时候进行转换
      case Branch(left, right) => mb.op(foldMap(left)(f)(mb), foldMap(right)(f)(mb)) //递归组合各个叶子节点，根据幺半群结合律法则，对半查找和从头编列结合的数据是一致的。
    }
  }

  /**
   * 练习10.14实现Option的Foladable实例
   */
  object OptionFoldable extends Foldable[Option] {
    override def foldRight[A, B](as: Option[A])(z: B)(f: (A, B) => B): B = as match {
      case None => z
      case Some(a) => f(a, z)
    }

    override def foldLeft[A, B](as: Option[A])(z: B)(f: (B, A) => B): B = as match {
      case None => z
      case Some(a) => f(z, a)
    }

    override def foldMap[A, B](as: Option[A])(f: A => B)(mb: Monoid[B]): B = as match {
      case None => mb.zero
      case Some(a) => f(a)
    }
  }
  def mapMergeMonoid[K, V](V: Monoid[V]): Monoid[Map[K, V]] = new Monoid[Map[K, V]] {
    override def op(a: Map[K, V], b: Map[K, V]): Map[K, V] = (a.keySet ++ b.keySet).foldLeft(zero) { (acc, k) =>
      acc.updated(k, V.op(a.getOrElse(k, V.zero), b.getOrElse(k, V.zero)))
    }

    override def zero: Map[K, V] = Map[K, V]()
  }

  /**
   * 练习10.17
   *
   * @param B
   * @tparam A
   * @tparam B
   * @return
   */
  def functionMonoid[A, B](B: Monoid[B]): Monoid[A => B] = new Monoid[A => B] {
    override def op(f: A => B, g: A => B): A => B = a => B.op(f(a), g(a))

    override def zero: A => B = a => B.zero
  }

  def bag[A](as: IndexedSeq[A]): Map[A, Int] = foldMapV(as, mapMergeMonoid[A, Int](intAddition))((a: A) => Map(a -> 1))


}
