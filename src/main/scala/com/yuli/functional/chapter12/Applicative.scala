package com.yuli.functional.chapter12

import com.yuli.functional.chapter10.Monoid
import com.yuli.functional.chapter10.Monoid.Foldable
import com.yuli.functional.chapter11.{Functor, Monad}
import com.yuli.functional.chapter3.Tree
import com.yuli.functional.chapter6.State

/**
 * 可应用函子，将map2和unit作为原语
 *
 * @tparam F
 */
trait Applicative[F[_]] extends Functor[F] {
  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] = apply(map(fa)(f.curried))(fb)

  def unit[A](a: => A): F[A]

  def apply[A, B](fab: F[A => B])(fa: F[A]): F[B] = map2(fab, fa)(_ (_))

  /**
   * map
   *
   * @param fa
   * @param f
   * @tparam A
   * @tparam B
   * @return
   */
  def map[A, B](fa: F[A])(f: A => B): F[B] = apply(unit(f))(fa)

  /**
   * 可以用这个函数
   *
   * @param as
   * @param f
   * @tparam A
   * @tparam B
   * @return
   */
  def traverse[A, B](as: List[A])(f: A => F[B]): F[List[B]] = as.foldRight(unit(List[B]()))((a, fbs) => map2(f(a), fbs)(_ :: _))

  /**
   * 使用traverse，转换同一个对象
   *
   * @param fas
   * @tparam A
   * @return
   */
  def sequence[A](fas: List[F[A]]): F[List[A]] = traverse(fas)(fa => fa)

  def replicateM[A](n: Int, fa: F[A]): F[List[A]] = sequence(List.fill(n)(fa))

  def factor[A, B](fa: F[A], fb: F[B]): F[(A, B)] = map2(fa, fb)((_, _))

  /**
   * 练习12.8
   *
   * @param G
   * @tparam G
   * @return
   */
  def product[G[_]](G: Applicative[G]): Applicative[({type f[x] = (F[x], G[x])})#f] = {
    val self = this
    new Applicative[({type f[x] = (F[x], G[x])})#f] {
      override def unit[A](a: => A): (F[A], G[A]) = (self.unit(a), G.unit(a))

      override def apply[A, B](fab: (F[A => B], G[A => B]))(fa: (F[A], G[A])): (F[B], G[B]) = (self.apply(fab._1)(fa._1), G.apply(fab._2)(fa._2))
    }
  }

  /**
   * 练习12.9
   *
   * @param G
   * @tparam G
   * @return
   */
  def compose[G[_]](G: Applicative[G]): Applicative[({type f[x] = F[G[x]]})#f] = {
    val self = this
    new Applicative[({type f[x] = F[G[x]]})#f] {
      override def unit[A](a: => A): F[G[A]] = self.unit(G.unit(a))

      override def map2[A, B, C](fa: F[G[A]], fb: F[G[B]])(f: (A, B) => C): F[G[C]] = self.map2(fa, fb)(G.map2(_, _)(f))
    }
  }

  def sequenceMap[K, V](ofa: Map[K, F[V]]): F[Map[K, V]] = (ofa foldLeft (unit(Map.empty[K, V]))) { case (acc, (k, fv)) => map2(acc, fv)((m, v) => m + (k -> v)) }

  /**
   * 练习12.3
   *
   * @param fa
   * @param fb
   * @param fc
   * @param f
   * @tparam A
   * @tparam B
   * @tparam C
   * @tparam D
   * @return
   */
  def map3[A, B, C, D](fa: F[A], fb: F[B], fc: F[C])(f: (A, B, C) => D): F[D] = apply(apply(apply(unit(f.curried))(fa))(fb))(fc)

  def map4[A, B, C, D, E](fa: F[A], fb: F[B], fc: F[C], fd: F[D])(f: (A, B, C, D) => E): F[E] = apply(apply(apply(apply(unit(f.curried))(fa))(fb))(fc))(fd)
}

sealed trait Validation[+E, +A]

case class Failure[E](head: E, tail: Vector[E]) extends Validation[E, Nothing]

case class Success[A](a: A) extends Validation[Nothing, A]


object Applicative {
  val streamApplicative = new Applicative[LazyList] {
    override def unit[A](a: => A): LazyList[A] = LazyList.continually(a) //无限常量流

    override def map2[A, B, C](fa: LazyList[A], fb: LazyList[B])(f: (A, B) => C): LazyList[C] = fa zip fb map f.tupled

    def eitherMonad[E]: Monad[({type f[x] = Either[E, x]})#f] = new Monad[({
      type f[x] = Either[E, x]
    })#f] {
      override def flatMap[A, B](fa: Either[E, A])(f: A => Either[E, B]): Either[E, B] = fa match {
        case Right(a) => f(a)
        case Left(e) => Left(e)
      }

      override def unit[A](a: => A): Either[E, A] = Right(a)
    }
  }

  def validationApplicative[E]: Applicative[({type f[x] = Validation[E, x]})#f] = new Applicative[({
    type f[x] = Validation[E, x]
  })#f] {
    override def unit[A](a: => A): Validation[E, A] = Success(a)

    override def map2[A, B, C](fa: Validation[E, A], fb: Validation[E, B])(f: (A, B) => C): Validation[E, C] = (fa, fb) match {
      case (Success(a), Success(b)) => Success(f(a, b))
      case (Failure(h1, t1), Failure(h2, t2)) => Failure(h1, t1 ++ Vector(h2) ++ t2)
      case (e@Failure(_, _), _) => e
      case (_, e@Failure(_, _)) => e
    }
  }
}

/**
 * 使用applicative实现monad
 *
 * @tparam F
 */
trait Monad[F[_]] extends Applicative[F] {
  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]

  def join[A](ffa: F[F[A]]): F[A] = flatMap(ffa)(fa => fa)

  def compose[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] = a => flatMap(f(a))(g)

  override def map[A, B](fa: F[A])(f: A => B): F[B] = flatMap(fa)(a => unit(f(a)))

  override def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] = flatMap(fa)(a => map(fb)(b => f(a, b)))
}

object Monad {
  def stateMonad[S] = new Monad[({type f[x] = State[S, x]})#f] {
    override def flatMap[A, B](fa: State[S, A])(f: A => State[S, B]): State[S, B] = fa flatMap f

    override def unit[A](a: => A): State[S, A] = State(s => (a, s))
  }

  /**
   * 练习12.20
   * @param G
   * @param H
   * @param T
   * @tparam G
   * @tparam H
   * @return
   */
  def composeM[G[_], H[_]](implicit G: Monad[G], H: Monad[H], T: Traversable[H]): Monad[({type f[x] = G[H[x]]})#f] = new Monad[({type f[x] = G[H[x]]})#f] {
    override def flatMap[A, B](fa: G[H[A]])(f: A => G[H[B]]): G[H[B]] = G.flatMap(fa)(na => G.map(T.traverse(na)(f))(H.join))

    override def unit[A](a: => A): G[H[A]] = G.unit(H.unit(a))
  }
}

trait Traversable[F[_]] extends Functor[F] with Foldable[F] {
  self =>
  def traverse[G[_] : Applicative, A, B](fa: F[A])(f: A => G[B]): G[F[B]] = sequence(map(fa)(f))

  def sequence[G[_] : Applicative, A](fga: F[G[A]]): G[F[A]] = traverse(fga)(ga => ga)

  type Id[A] = A
  val idMonad = new Monad[Id] {
    override def flatMap[A, B](fa: Id[A])(f: A => Id[B]): Id[B] = f(fa)

    override def unit[A](a: => A): Id[A] = a
  }

  /**
   * 练习12.14
   *
   * @param fa
   * @param f
   * @tparam A
   * @tparam B
   * @return
   */
  def map[A, B](fa: F[A])(f: A => B): F[B] = traverse[Id, A, B](fa)(f)(idMonad)

  type Const[M, B] = M

  implicit def monoidApplicative[M](M: Monoid[M]) = new Applicative[({type f[x] = Const[M, x]})#f] {
    override def unit[A](a: => A): Const[M, A] = M.zero

    override def map2[A, B, C](fa: Const[M, A], fb: Const[M, B])(f: (A, B) => C): Const[M, C] = M.op(fa, fb)
  }


  import State._

  override def foldMap[A, B](as: F[A])(f: A => B)(mb: Monoid[B]): B = traverse[({type f[x] = Const[B, x]})#f, A, Nothing](as)(f)(monoidApplicative(mb))

  def traverseS[S, A, B](fa: F[A])(f: A => State[S, B]): State[S, F[B]] = traverse[({type f[x] = State[S, x]})#f, A, B](fa)(f)(Monad.stateMonad)

  /**
   * 对每个元素生成序号
   *
   * @param ta
   * @tparam A
   * @return
   */
  def zipWithIndex_[A](ta: F[A]): F[(A, Int)] =
    traverseS(ta)((a: A) => (for {
      i <- get[Int]
      _ <- set(i + 1)
    } yield (a, i))).run(0)._1

  def toList_[A](as: F[A]): List[A] = traverseS(as)((a: A) => (for {
    as <- get[List[A]] //获取当前状态，累加的list
    _ <- set(a :: as)
  } yield ())).run(Nil)._2.reverse

  def mapAccum[S, A, B](fa: F[A], s: S)(f: (A, S) => (B, S)): (F[B], S) = traverseS(fa)((a: A) => (for {
    s1 <- get[S]
    (b, s2) = f(a, s1)
    _ <- set(s2)
  } yield b)).run(s)

  /**
   * 练习10.15 将Foldable结构转换为List
   *
   * @param as
   * @tparam A
   * @return
   */
  override def toList[A](as: F[A]): List[A] = mapAccum(as, List[A]())((a, s) => ((), a :: s))._2.reverse

  /**
   * 练习12.16 将函子转换为list后取反
   *
   * @param fa
   * @tparam A
   * @return
   */
  def reverse[A](fa: F[A]): F[A] = mapAccum(fa, toList(fa).reverse)((_, as) => (as.head, as.tail))._1

  override def foldRight[A, B](as: F[A])(z: B)(f: (A, B) => B): B = mapAccum(as, z)((a, b) => ((), f(a, b)))._2

  override def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) => B): B = mapAccum(as, z)((a, b) => ((), f(b, a)))._2

  def zip[A, B](fa: F[A], fb: F[B]): F[(A, B)] = (mapAccum(fa, toList(fb)) {
    case (a, Nil) => sys.error("zip不能处理改情况")
    case (a, b :: bs) => ((a, b), bs)
  }._1)

  def zipL[A, B](fa: F[A], fb: F[B]): F[(A, Option[B])] = (mapAccum(fa, toList(fb)) {
    case (a, Nil) => ((a, None), Nil)
    case (a, b :: bs) => ((a, Some(b)), bs)
  })._1

  def zipR[A, B](fa: F[A], fb: F[B]): F[(Option[A], B)] = (mapAccum(fb, toList(fa)) {
    case (b, Nil) => ((None, b), Nil)
    case (b, a :: as) => ((Some(a), b), as)
  })._1

  /**
   * 练习12.18 将两个可遍历函子融合成一个
   *
   * @param fa
   * @param f
   * @param g
   * @param G
   * @param H
   * @param M
   * @param N
   * @tparam G
   * @tparam H
   * @tparam A
   * @tparam B
   * @return
   */
  def fuse[G[_], H[_], A, B](fa: F[A])(f: A => G[B], g: A => H[B])(G: Applicative[G], H: Applicative[H])(implicit M: Applicative[G], N: Applicative[G]): (G[F[B]], H[F[B]]) =
    traverse[({type f[x] = (G[x], H[x])})#f, A, B](fa)(a => (f(a), g(a)))(G product H)

  def compose[G[_]](implicit G: Traversable[G]): Traversable[({type f[x] = F[G[x]]})#f] =
    new Traversable[({type f[x] = F[G[x]]})#f] {
      override def traverse[M[_]:Applicative,A,B](fa: F[G[A]])(f: A => M[B]) =
        self.traverse(fa)((ga: G[A]) => G.traverse(ga)(f))
    }

}

case class Tree[+A](head: A, tail: List[Tree[A]])

object Traversable {
  /**
   * 练习12.13
   */
  val listTraversable = new Traversable[List] {
    override def traverse[G[_], A, B](fa: List[A])(f: A => G[B])(implicit G: Applicative[G]): G[List[B]] =
      fa.foldRight(G.unit(List[B]()))((a, fbs) => G.map2(f(a), fbs)(_ :: _))
  }

  val optionTraversable = new Traversable[Option] {
    override def traverse[G[_], A, B](fa: Option[A])(f: A => G[B])(implicit G: Applicative[G]): G[Option[B]] = fa match {
      case Some(a) => G.map(f(a))(Some(_))
      case None => G.unit(None)
    }
  }
  val treeTraversable = new Traversable[Tree] {
    override def traverse[G[_], A, B](fa: Tree[A])(f: A => G[B])(implicit G: Applicative[G]): G[Tree[B]] = G.map2(f(fa.head), listTraversable.traverse(fa.tail)(a => traverse(a)(f)))(Tree(_, _))
  }
}