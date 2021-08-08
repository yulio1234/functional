package com.yuli.functional.chapter6

import com.yuli.functional.chapter11.Monad
import com.yuli.functional.chapter6.State.unit

/**
 * 抽象通用状态
 *
 * @param run
 * @tparam S
 * @tparam A
 */
case class State[S, +A](run: S => (A, S)) {
  def apply(stateMonad: Monad[({
    type lambda[X] = State[Nothing, X]
  })#lambda] with Object {
    def unit[A](a: => A): State[Nothing, A]

    def flatMap[A, B](ma: State[Nothing, A])(f: A => State[Nothing, B]): State[Nothing, B]
  }): Int = ???

  def flatMap[B](f: A => State[S, B]): State[S, B] = State { s =>
    val (a, s2) = run(s)
    f(a).run(s2)
  }

  def map[B](f: A => B): State[S, B] = flatMap(a => unit(f(a)))

  def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] = flatMap(a => sb.map(b => f(a, b)))
}

object State {

  def unit[S, A](a: A): State[S, A] = State(s => (a, s))

  def sequence[S, A](sas: List[State[S, A]]): State[S, List[A]] = sas.foldRight(unit[S, List[A]](Nil))((s, b) => s.map2(b)(_ :: _))

  def modify[S](f: S => S): State[S, Unit] = for {
    s <- get
    _ <- set(f(s))
  } yield ()

  def get[S]: State[S, S] = State(s => (s, s))

  def set[S](s: S): State[S, Unit] = State(_ => ((), s))
}
