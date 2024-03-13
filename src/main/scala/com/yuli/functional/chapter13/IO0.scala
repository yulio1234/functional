package com.yuli.functional.chapter13

import com.yuli.functional.chapter12.Monad
import com.yuli.functional.chapter13.IO0.fahrenheitToCelsius

import scala.io.StdIn.readLine

/**
 * 描述：io类型1,只能处理默认输出
 *
 * @author yuli
 * @date 2021/8/9
 */
object IO0 {
  trait IO {
    self =>
    def run(): Unit

    def ++(io: IO): IO = new IO {
      override def run(): Unit = {
        self.run()
        io.run()
      }
    }
  }

  object IO {
    def empty: IO = new IO {
      override def run: Unit = ()
    }
  }

  def fahrenheitToCelsius(f: Double): Double =
    (f - 32) * 5.0 / 9.0
}

/**
 * 支持输入对IO类型
 */
object IO1 {
  trait IO[A] {
    self =>
    def run: A

    def map[B](f: A => B): IO[B] = new IO[B] {
      override def run: B = f(self.run)
    }

    def flatMap[B](f: A => IO[B]): IO[B] = new IO[B] {
      override def run: B = f(self.run).run
    }
  }

  object IO extends Monad[IO] {
    override def flatMap[A, B](fa: IO[A])(f: A => IO[B]): IO[B] = fa.flatMap(f)

    override def unit[A](a: => A): IO[A] = new IO[A] {
      override def run: A = a
    }

    def apply[A](a: => A): IO[A] = unit(a)
  }

  def ReadLine: IO[String] = IO {
    readLine()
  }

  def PrintLine(message: String): IO[Unit] = IO {
    println(message)
  }

  def converter: IO[Unit] = for {
    _ <- PrintLine("输入一个温度")
    d <- ReadLine.map(_.toDouble)
    _ <- PrintLine(fahrenheitToCelsius(d).toString)
  } yield ()

  /**
   * 从命令行读取一行并输出
   */
  val echo: IO[Unit] = ReadLine.flatMap(PrintLine)
  /**
   * 从命令行读取一行并解析成Int返回
   */
  val readInt: IO[Int] = ReadLine.map(_.toInt)
}

object IO2 {
  sealed trait IO[A] {
    def flatMap[B](f: A => IO[B]): IO[B] = FlatMap(this, f)

    def map[B](f: A => B): IO[B] = flatMap(f andThen (Return(_)))
  }

  /**
   * 单纯返回数据
   * @param a
   * @tparam A
   */
  case class Return[A](a: A) extends IO[A]

  /**
   * 执行作用，并返回参数
   * @param resume
   * @tparam A
   */
  case class Suspend[A](resume: () => A) extends IO[A]

  /**
   * 两个步骤组合
   * @param sub
   * @param k
   * @tparam A
   * @tparam B
   */
  case class FlatMap[A, B](sub: IO[A], k: A => IO[B]) extends IO[A]

  def printLine(s:String):IO[Unit] = Suspend(()=>Return(println(s)))

//  def run[A](io:IO[A]):A = io match {
//    case Return(a) => a
//    case Suspend(r) =>r()
//    case FlatMap(x,f) => x match {
//      case Return(a) => run(f(a))
//      case Suspend(r) => run(f(r()))
//      case FlatMap(y,g) => run(y flatMap(a => g(a) flatMap(f)))
//
//    }
//
//  }
}
