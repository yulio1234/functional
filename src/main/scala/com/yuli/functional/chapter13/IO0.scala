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
    def run: Unit

    def ++(io: IO): IO = new IO {
      override def run: Unit = {
        self.run
        io.run
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
}
