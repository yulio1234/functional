package com.yuli.functional.chapter8

import com.yuli.functional.chapter6.RNG
import com.yuli.functional.chapter7.Par
import com.yuli.functional.chapter7.Par.{Par, equal}
import com.yuli.functional.chapter8.Gen.{choose, unit, weighted}
import com.yuli.functional.chapter8.Prop.{FailedCase, MaxSize, SuccessCount, TestCases, forAll}

import java.util.concurrent.{ExecutorService, Executors}


sealed trait Result {
  def isFalsified: Boolean
}

case object Passed extends Result {
  override def isFalsified: Boolean = false
}

case class Falsified(failure: FailedCase, successes: SuccessCount) extends Result {
  override def isFalsified: Boolean = true
}

case object Proved extends Result {
  override def isFalsified: Boolean = false
}

case class Prop(run: (MaxSize, TestCases, RNG) => Result) {


  def &&(p: Prop): Prop = Prop {
    (max, n, rng) =>
      run(max, n, rng) match {
        //成功才继续执行
        case Passed | Proved => p.run(max, n, rng)
        case x => x
      }
  }

  def ||(p: Prop): Prop = Prop {
    (max, n, rng) =>
      run(max, n, rng) match {
        //如果失败才继续执行，成功就返回
        case Falsified(msg, _) => p.tag(msg).run(max, n, rng)
        case x => x
      }
  }

  def tag(msg: String): Prop = Prop {
    (max, n, rng) =>
      run(max, n, rng) match {
        case Falsified(e, c) => Falsified(msg + "\n" + e, c)
      }
  }
}


object Prop {
  type TestCases = Int
  type FailedCase = String
  type SuccessCount = Int
  type MaxSize = Int

  def check(p: => Boolean): Prop = Prop { (_, _, _) =>
    if (p) Proved else Falsified("()", 0)
  }

  def checkPar: Unit = {
    //    equal(Par.map(Par.unit(1))(_+1),Par.unit(2))
  }

  def run(p: Prop, maxSize: Int = 100, testCases: Int = 100, rng: RNG = RNG.SimpleRNG(System.currentTimeMillis)): Unit =
    p.run(maxSize, testCases, rng) match {
      case Falsified(msg, n) => println(s"! Falsified after $n passed tests:\n $msg")
      case Passed => println(s"+ Ok,passed $testCases tests.")
      case Proved => println(s"+ Ok, proved property")
    }

  def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop {
    (max, n, rng) =>
      randomStream(as)(rng).zip(LazyList.from(0)).take(n).map {
        case (a, i) => try {
          if (f(a)) Passed else Falsified(a.toString, i)
        } catch {
          case e: Exception => Falsified(buildMsg(a, e), i)
        }
      }.find(_.isFalsified).getOrElse(Passed)

  }

  def forAll[A](g: SGen[A])(f: A => Boolean): Prop = forAll(g(_))(f)

  /**
   * 慢慢缩小用例范围
   *
   * @param g
   * @param f
   * @tparam A
   * @return
   */
  def forAll[A](g: Int => Gen[A])(f: A => Boolean): Prop = Prop {
    (max, n, rng) =>
      //根据size生成对应的随机用例
      val casesSize = (n + (max - 1)) / max
      //每个长度创建一个性质，但不超过n个
      val props = LazyList.from(0).take((n min max) + 1).map(i => {
        forAll(g(i))(f)
      })
      //不加这一行，就只能返回size为1的数组，加了才会返回多个
      val prop = props.map(p => Prop {
        (max, _, rng) =>
          p.run(max, casesSize, rng)
      }).toList.reduce(_ && _)
      prop.run(max, n, rng)
  }

  private val S: Gen[ExecutorService] = weighted(choose(1, 4).map(Executors.newFixedThreadPool) -> .75, unit(Executors.newCachedThreadPool) -> .25)

  def forAllPar[A](g: Gen[A])(f: A => Par[Boolean]): Prop = forAll(S ** g) { case (s, a) => f(a)(s).get() }

  /**
   * 通过简单重复调用生成器实现一个无限的A Stream
   *
   * @param g
   * @param rng
   * @tparam A
   * @return
   */
  def randomStream[A](g: Gen[A])(rng: RNG): LazyList[A] = LazyList.unfold(rng)(rng => Some(g.simple.run(rng)))

  /**
   * 构建异常信息
   *
   * @param s
   * @param e
   * @tparam A
   * @return
   */
  def buildMsg[A](s: A, e: Exception): String = s"test case $s\n" + s"generated an exception: ${e.getMessage}\n" + s"stack trace:\n ${e.getStackTrace.mkString("\n")}"
}

