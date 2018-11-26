package fpinscala
package applicative

import fpinscala.applicative.StateUtil._
import fpinscala.monads.{Functor, Monad}
import fpinscala.monoids._
import fpinscala.state._

import scala.language.{higherKinds, implicitConversions}

trait Applicative[F[_]] extends Functor[F] { self =>

  // primitive combinators
  def map2[A,B,C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] = apply(apply(unit(f.curried))(fa))(fb)

  def unit[A](a: => A): F[A]

  // derived combinators
  def apply[A,B](fab: F[A => B])(fa: F[A]): F[B] = map2(fab, fa)((aToB, a) => aToB(a))

  def map[A,B](fa: F[A])(f: A => B): F[B] = apply(unit(f))(fa)

  def sequence[A](fas: List[F[A]]): F[List[A]] = traverse(fas)(identity)

  def traverse[A,B](as: List[A])(f: A => F[B]): F[List[B]] =
    as.foldRight(unit(List[B]()))((a, acc) => map2(f(a), acc)(_ :: _))

  def replicateM[A](n: Int, fa: F[A]): F[List[A]] = traverse(List.fill(n)(fa))(identity)

  def factor[A,B](fa: F[A], fb: F[B]): F[(A,B)] = map2(fa, fb)((_, _))

  def product[G[_]](G: Applicative[G]): Applicative[({type f[x] = (F[x], G[x])})#f] = new Applicative[({type f[x] = (F[x], G[x])})#f] {
    override def unit[A](a: => A): (F[A], G[A]) = (self.unit(a), G.unit(a))

    override def map2[A, B, C](fa: (F[A], G[A]), fb: (F[B], G[B]))(f: (A, B) => C): (F[C], G[C]) =
      (self.map2(fa._1, fb._1)(f), G.map2(fa._2, fb._2)(f))
  }

  def compose[G[_]](G: Applicative[G]): Applicative[({type f[x] = F[G[x]]})#f] = new Applicative[({type f[x] = F[G[x]]})#f] {
    override def unit[A](a: => A): F[G[A]] = self.unit(G.unit(a))

    override def map2[A, B, C](fga: F[G[A]], fgb: F[G[B]])(f: (A, B) => C): F[G[C]] =
      self.map2(fga, fgb)((ga, gb) => G.map2(ga, gb)(f))
  }

  def sequenceMap[K,V](ofa: Map[K,F[V]]): F[Map[K,V]] = traverseMap(ofa)(identity)

  def traverseMap[K, A, B](mka: Map[K, A])(f: A => F[B]): F[Map[K, B]] =
    mka.foldRight(unit(Map[K, B]())) { case ((k, a), acc) => map2(acc, f(a))((accMap, b) => accMap + (k -> b)) }

  def map3[A, B, C, D](fa: F[A], fb: F[B], fc: F[C])(f: (A, B, C) => D): F[D] =
    apply(apply(apply(unit(f.curried))(fa))(fb))(fc)

  def map4[A, B, C, D, E](fa: F[A], fb: F[B], fc: F[C], fd: F[D])(f: (A, B, C, D) => E): F[E] =
    apply(apply(apply(apply(unit(f.curried))(fa))(fb))(fc))(fd)
}

case class Tree[+A](head: A, tail: List[Tree[A]])

trait Monad[F[_]] extends Applicative[F] {
  def flatMap[A,B](ma: F[A])(f: A => F[B]): F[B] = join(map(ma)(f))

  def join[A](mma: F[F[A]]): F[A] = flatMap(mma)(ma => ma)

  override def map[A, B](m: F[A])(f: A => B): F[B] = flatMap(m)(a => unit(f(a)))

  def compose[A,B,C](f: A => F[B], g: B => F[C]): A => F[C] =
    a => flatMap(f(a))(g)

  override def apply[A,B](mf: F[A => B])(ma: F[A]): F[B] =
    flatMap(mf)(f => map(ma)(a => f(a)))
}

object Monad {
  def eitherMonad[E]: Monad[({type f[x] = Either[E, x]})#f] = new Monad[({type f[x] = Either[E, x]})#f] {
    override def unit[A](a: => A): Either[E, A] = Right(a)

    override def flatMap[A, B](ma: Either[E, A])(f: A => Either[E, B]): Either[E, B] = ma match {
      case Right(a) => f(a)
      case Left(x) => Left(x)
    }
  }

  val optionMonad: Monad[Option] = new Monad[Option] {
    override def flatMap[A, B](ma: Option[A])(f: A => Option[B]): Option[B] = ma flatMap f

    override def unit[A](a: => A): Option[A] = Option(a)
  }

  val listMonad: Monad[List] = new Monad[List] {
    override def flatMap[A, B](ma: List[A])(f: A => List[B]): List[B] = ma flatMap f

    override def unit[A](a: => A): List[A] = List(a)
  }


  def stateMonad[S] = new Monad[({type f[x] = State[S, x]})#f] {
    def unit[A](a: => A): State[S, A] = State(s => (a, s))
    override def flatMap[A,B](st: State[S, A])(f: A => State[S, B]): State[S, B] =
      st flatMap f
  }

  def composeM[F[_],N[_]](implicit F: Monad[F], N: Monad[N], T: Traverse[N]): Monad[({type f[x] = F[N[x]]})#f] =
    new Monad[({type f[x] = F[N[x]]})#f] {
      override def unit[A](a: => A): F[N[A]] = F.unit(N.unit(a))
      override def flatMap[A, B](ma: F[N[A]])(f: A => F[N[B]]): F[N[B]] =
        F.flatMap(ma)(na => F.map(T.traverse(na)(f))(N.join))
    }
}

sealed trait Validation[+E, +A]

case class Failure[E](head: E, tail: Vector[E])
  extends Validation[E, Nothing]

case class Success[A](a: A) extends Validation[Nothing, A]


object Applicative {

  val streamApplicative = new Applicative[Stream] {

    def unit[A](a: => A): Stream[A] =
      Stream.continually(a) // The infinite, constant stream

    override def map2[A,B,C](a: Stream[A], b: Stream[B])( // Combine elements pointwise
                    f: (A,B) => C): Stream[C] =
      a zip b map f.tupled
  }

  def validationApplicative[E]: Applicative[({type f[x] = Validation[E, x]})#f] = new Applicative[({type f[x] = Validation[E, x]})#f] {
    override def unit[A](a: => A): Validation[E, A] = Success(a)

    override def map2[A, B, C](fa: Validation[E, A], fb: Validation[E, B])(f: (A, B) => C): Validation[E, C] = (fa, fb) match {
      case (Success(a), Success(b)) => Success(f(a, b))
      case (Failure(h1, t1), Failure(h2, t2)) => Failure(h2, t2 ++ (h1 +: t1))
      case (_, f@Failure(_, _)) => f
      case (f@Failure(_, _), _) => f
    }
  }

  type Const[A, B] = A

  implicit def monoidApplicative[M](M: Monoid[M]): Applicative[({ type f[x] = Const[M, x] })#f] =
    new Applicative[({ type f[x] = Const[M, x] })#f] {
      def unit[A](a: => A): M = M.zero
      override def apply[A,B](m1: M)(m2: M): M = M.op(m1, m2)
    }

  val listApplicative: Applicative[List] = new Applicative[List] {
    override def unit[A](a: => A): List[A] = List(a)

    override def map2[A, B, C](fa: List[A], fb: List[B])(f: (A, B) => C): List[C] = fa zip fb map f.tupled
  }

  val optionApplicative: Applicative[Option] = new Applicative[Option] {
    override def unit[A](a: => A): Option[A] = Option(a)

    override def map2[A, B, C](fa: Option[A], fb: Option[B])(f: (A, B) => C): Option[C] = for {
      a <- fa
      b <- fb
    } yield f(a, b)
  }

}

trait Traverse[F[_]] extends Functor[F] with Foldable[F] { self =>
  def traverse[G[_]:Applicative,A,B](fa: F[A])(f: A => G[B]): G[F[B]] = sequence(map(fa)(f))
  def sequence[G[_],A](fma: F[G[A]])(implicit G: Applicative[G]): G[F[A]] = traverse(fma)(ma => ma)

  type Id[A] = A
  val idApplicative: Applicative[Id] = new Applicative[Id] {
    override def unit[A](a: => A): Id[A] = a
    override def map2[A, B, C](fa: Id[A], fb: Id[B])(f: (A, B) => C): Id[C] = f(fa, fb)
  }
  def map[A,B](fa: F[A])(f: A => B): F[B] = traverse[Id, A, B](fa)(f)(idApplicative)

  import Applicative._

  override def foldMap[A,B](as: F[A])(f: A => B)(mb: Monoid[B]): B = traverse[({type f[x] = Const[B,x]})#f,A,Nothing](as)(f)(monoidApplicative(mb))

  def traverseS[S,A,B](fa: F[A])(f: A => State[S, B]): State[S, F[B]] =
    traverse[({type f[x] = State[S,x]})#f,A,B](fa)(f)(Monad.stateMonad)

  def mapAccum[S,A,B](fa: F[A], s: S)(f: (A, S) => (B, S)): (F[B], S) =
    traverseS(fa)((a: A) => (for {
      s1 <- get[S]
      (b, s2) = f(a, s1)
      _  <- set(s2)
    } yield b)).run(s)

  override def toList[A](fa: F[A]): List[A] =
    mapAccum(fa, List[A]())((a, s) => ((), a :: s))._2.reverse

  def zipWithIndex[A](fa: F[A]): F[(A, Int)] =
    mapAccum(fa, 0)((a, s) => ((a, s), s + 1))._1

  def reverse[A](fa: F[A]): F[A] = mapAccum(fa, toList(fa).reverse)((_, as) => (as.head, as.tail))._1

  override def foldLeft[A, B](fa: F[A])(z: B)(f: (B, A) => B): B = mapAccum(fa, z)((a, s) => ((), f(s, a)))._2

  def fuse[G[_], H[_], A, B](fa: F[A])(f: A => G[B], g: A => H[B])
                            (implicit G: Applicative[G], H: Applicative[H]): (G[F[B]], H[F[B]]) =
    traverse[({type f[x] = (G[x], H[x])})#f, A, B](fa)(a => (f(a), g(a)))(G product H)

  def compose[G[_]](implicit G: Traverse[G]): Traverse[({type f[x] = F[G[x]]})#f] = new Traverse[({type f[x] = F[G[x]]})#f] {
    override def traverse[H[_]: Applicative, A, B](fa: F[G[A]])(f: A => H[B]): H[F[G[B]]] =
      self.traverse(fa)((ga: G[A]) => G.traverse(ga)(f))
  }
}

object Traverse {
  val listTraverse = new Traverse[List] {
    override def traverse[G[_], A, B](fa: List[A])(f: A => G[B])(implicit G: Applicative[G]): G[List[B]] =
      fa.foldRight(G.unit(List[B]()))((a, acc) => G.map2(f(a), acc)(_ :: _))
  }

  val optionTraverse = new Traverse[Option] {
    override def traverse[G[_], A, B](fa: Option[A])(f: A => G[B])(implicit G: Applicative[G]): G[Option[B]] = fa match {
      case Some(a) => G.map(f(a))(Some(_))
      case None => G.unit(None)
    }
  }

  val treeTraverse = new Traverse[Tree] {
    override def traverse[G[_], A, B](fa: Tree[A])(f: A => G[B])(implicit G: Applicative[G]): G[Tree[B]] = fa match {
      case Tree(head, tail) => G.map2(f(head), listTraverse.traverse(tail)(tree => traverse(tree)(f)(G))(G))(Tree(_, _))
    }
  }
}

// The `get` and `set` functions on `State` are used above,
// but aren't in the `exercises` subproject, so we include
// them here
object StateUtil {

  def get[S]: State[S, S] =
    State(s => (s, s))

  def set[S](s: S): State[S, Unit] =
    State(_ => ((), s))
}
