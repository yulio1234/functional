package com.yuli.functional.chapter11

import com.yuli.functional.chapter6.State
import com.yuli.functional.chapter7.Nonblocking.Par
import com.yuli.functional.chapter8.Gen
import com.yuli.functional.chapter9.Parsers

/**
 * 函子
 * map(x)(a => a) == x
 *
 * @tparam F
 */
trait Functor[F[_]] {
  def map[A, B](fa: F[A])(f: A => B): F[B]

  /**
   * 分派,相当于泛化unzip
   *
   * @param fab
   * @tparam A
   * @tparam B
   * @return
   */
  def distribute[A, B](fab: F[(A, B)]): (F[A], F[B]) = (map(fab)(_._1), map(fab)(_._2))

  def codistribute[A, B](e: Either[F[A], F[B]]): F[Either[A, B]] = e match {
    case Left(fa) => map(fa)(Left(_))
    case Right(fb) => map(fb)(Right(_))
  }
}

object Functor {
  new Functor[List] {
    override def map[A, B](as: List[A])(f: A => B): List[B] = as map f

  }
}

/**
 * 单子
 * 结合律：compose(compose(f,g),h) == compose(f,compose(g,h))
 * 单位元：compose(f,unit) == f;compose(unit,f) == f
 *
 * @tparam F
 */
trait Monad[F[_]] extends Functor[F] {
  def unit[A](a: => A): F[A]

  def flatMap[A, B](ma: F[A])(f: A => F[B]): F[B]

  def map[A, B](ma: F[A])(f: A => B): F[B] = flatMap(ma)(a => unit(f(a)))

  def map2[A, B, C](ma: F[A], mb: F[B])(f: (A, B) => C): F[C] = flatMap(ma)(a => map(mb)(b => f(a, b)))

  /**
   * 练习11.3
   *
   * @param lma
   *
   * @tparam A
   * @return
   */
  def sequence[A](lma: List[F[A]]): F[List[A]] = lma.foldRight(unit(List[A]()))((ma, mla) => map2(ma, mla)(_ :: _))

  def traverse[A, B](la: List[A])(f: A => F[B]): F[List[B]] = la.foldRight(unit(List[B]()))((a, mlb) => map2(f(a), mlb)(_ :: _))

  /**
   * 练习11.4
   *
   * @param n
   * @param ma
   * @tparam A
   * @return
   */
  def replicateM[A](n: Int, ma: F[A]): F[List[A]] = sequence(List.fill(n)(ma))

  def product[A, B](ma: F[A], mb: F[B]): F[(A, B)] = map2(ma, mb)((_, _))

  /**
   * 练习11.7
   *
   * @param f
   * @param g
   * @tparam A
   * @tparam B
   * @tparam C
   * @return
   */
  def compose[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] = a => flatMap(f(a))(g)

  /**
   * 练习11.8
   *
   * @param ma
   * @param f
   * @tparam A
   * @tparam B
   * @return
   */
  def flatMapViaCompose[A, B](ma: F[A])(f: A => F[B]): F[B] = compose((_: Unit) => ma, f)(())

  def filterM[A](ms: List[A])(f: A => F[Boolean]): F[List[A]] = ms.foldRight(unit(List[A]()))((x, y) => compose(f, (b: Boolean) => if (b) map2(unit(x), y)(_ :: _) else y)(x))

  def join[A](mma: F[F[A]]): F[A] = flatMap(mma)(ma => ma)

}

object Monad {
  val genMonad = new Monad[Gen] {
    override def unit[A](a: => A): Gen[A] = Gen.unit(a)

    /**
     * 符合法则 x.flatMap(f).flatMap(p) = x.flatMap(a => f(a).flatMap(p))
     *
     * @param ma
     * @param f
     * @tparam A
     * @tparam B
     * @return
     */
    override def flatMap[A, B](ma: Gen[A])(f: A => Gen[B]): Gen[B] = ma.flatMap(f)
  }
  /**
   * 练习11.1
   */
  val parMonad = new Monad[Par] {
    override def unit[A](a: => A): Par[A] = Par.unit(a)

    override def flatMap[A, B](ma: Par[A])(f: A => Par[B]): Par[B] = Par.flatMap(ma)(f)
  }

  def parserMonad[P[+_]](p: Parsers[P]): Monad[P] = new Monad[P] {
    override def unit[A](a: => A): P[A] = p.succeed(a)

    override def flatMap[A, B](ma: P[A])(f: A => P[B]): P[B] = p.flatMap(ma)(f)
  }

  val optionMonad = new Monad[Option] {
    override def unit[A](a: => A): Option[A] = Some(a)

    override def flatMap[A, B](ma: Option[A])(f: A => Option[B]): Option[B] = ma.flatMap(f)
  }
  val streamMonad = new Monad[LazyList] {
    override def unit[A](a: => A): LazyList[A] = LazyList(a)

    override def flatMap[A, B](ma: LazyList[A])(f: A => LazyList[B]): LazyList[B] = ma flatMap f
  }
  val listMonad = new Monad[List] {
    override def unit[A](a: => A): List[A] = List(a)

    override def flatMap[A, B](ma: List[A])(f: A => List[B]): List[B] = ma flatMap f
  }

  def stateMonad[S] = new Monad[({type lambda[X] = State[S, X]})#lambda] {
    override def unit[A](a: => A): State[S, A] = State(s => (a, s))

    override def flatMap[A, B](ma: State[S, A])(f: A => State[S, B]): State[S, B] = ma flatMap f
  }

  val idMonad = new Monad[Id] {
    override def unit[A](a: => A): Id[A] = Id(a)

    override def flatMap[A, B](ma: Id[A])(f: A => Id[B]): Id[B] = ma flatMap f
  }
  type IntState[A] = State[Int, A]

  def getState[S]: State[S, S] = State(s => (s, s))

  def setState[S](s: => S): State[S, Unit] = State(_ => ((), s))

  val F = stateMonad[Int]

  def zipWithIndex[A](as: List[A]): List[(Int, A)] = as.foldLeft(F.unit(List[(Int, A)]()))((acc, a) => for {
    xs <- acc
    n <- getState
    _ <- setState(n + 1)
  } yield (n, a) :: xs).run(0)._1.reverse

  def readerMonad[R] = new Monad[({type f[X] = Reader[R, X]})#f] {
    override def unit[A](a: => A): Reader[R, A] = Reader(_ => a)

    override def flatMap[A, B](ma: Reader[R, A])(f: A => Reader[R, B]): Reader[R, B] = Reader(r => f(ma.run(r)).run(r))
  }
}

case class Id[A](value: A) {
  def map[B](f: A => B): Id[B] = Id(f(value))

  def flatMap[B](f: A => Id[B]): Id[B] = f(value)
}

case class Reader[R, A](run: R => A)
