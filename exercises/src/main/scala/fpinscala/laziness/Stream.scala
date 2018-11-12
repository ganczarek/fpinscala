package fpinscala.laziness

import fpinscala.laziness.Stream._

import scala.annotation.tailrec

trait Stream[+A] {

  def toList: List[A] = foldRight(Nil:List[A])(_ :: _)

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 1 => cons(h(), t().take(n - 1))
    case Cons(h, _) if n == 1 => cons(h(), empty)
    case _ => empty
  }

  @tailrec
  final def drop(n: Int): Stream[A] = this match {
    case Cons(_, t) if n > 0 => t().drop(n - 1)
    case _ => this
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => cons(h(), t() takeWhile p)
    case _ => empty
  }

  def forAll(p: A => Boolean): Boolean = this.foldRight(true)((a, b) => p(a) && b)

  def takeWhileWithFoldRight(p: A => Boolean): Stream[A] =
    this.foldRight(empty: Stream[A])((a, b) => if (p(a)) cons(a, b) else empty)

  def headOption: Option[A] =
    this.foldRight(None: Option[A])((a, _) => Some(a))

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

  def map[B](f: A => B): Stream[B] =
    this.foldRight(empty[B])((a, b) => cons(f(a), b))

  def filter(f: A => Boolean): Stream[A] = this.foldRight(empty[A])((a, b) => if (f(a)) cons(a, b) else b)

  def append[B >: A](b: => Stream[B]): Stream[B] =
    this.foldRight(b)((a, b) => cons(a, b))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    this.foldRight(empty[B])((a, b) => f(a) append b)

  def mapWithUnfold[B](f: A => B): Stream[B] = Stream.unfold(this) {
    case Cons(h, t) => Some((f(h()), t()))
    case _ => None
  }

  def takeWithUnfold(n: Int): Stream[A] = Stream.unfold((this, n)) {
    case (Cons(h, _), 1) => Some((h(), (empty, 0))) // avoid evaluating stream elements unnecessarily
    case (Cons(h, t), x) if x > 0 => Some((h(), (t(), x-1)))
    case _ => None
  }

  def takeWhileWithUnfold(p: A => Boolean): Stream[A] = Stream.unfold(this) {
    case Cons(h, t) if p(h()) => Some(h(), t())
    case _ => None
  }

  def zipWith[B](s: Stream[B]): Stream[(A, B)] = Stream.unfold((this, s)) {
    case (Cons(h1, t1), Cons(h2, t2)) => Some((h1(), h2()), (t1(), t2()))
    case _ => None
  }

  def zipAllWithUnfold[B](s: Stream[B]): Stream[(Option[A], Option[B])] = Stream.unfold((this, s)) {
    case (Empty, Empty) => None
    case (Cons(h1, t1), Cons(h2, t2)) => Some(((Some(h1()), Some(h2())), (t1(), t2())))
    case (Empty, Cons(h2, t2)) => Some(((None, Some(h2())), (Empty, t2())))
    case (Cons(h1, t1), Empty) => Some(((Some(h1()), None), (t1(), Empty)))
  }

  def startsWith[B](s: Stream[B]): Boolean =
    this zipAllWithUnfold s takeWhile (_._2.isDefined) forAll { case (a1, a2) => a1 == a2 }

  def tails: Stream[Stream[A]] = Stream.unfold(this) {
    case Empty => None
    case s => Some(s -> (s drop 1))
  } append Stream(Empty)

}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)

  def from(n: Int): Stream[Int] = cons(n, from(n + 1))

  def fibs(): Stream[Int] = {
    def fibs_1(a: Int, b: Int): Stream[Int] = cons(a, fibs_1(b, a + b))

    fibs_1(0, 1)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    f(z) map { case (value, state) => cons(value, unfold(state)(f)) } getOrElse empty[A]
  }

  def constant[A](a: A): Stream[A] = cons(a, constant(a))

  def fibs_1(): Stream[Int] = unfold((0, 1, 1))(state => Some((state._1, (state._2, state._3, state._2 + state._3))))

  def constant_1[A](a: A): Stream[A] = unfold(a)(s => Some(s, s))

  def ones_1(): Stream[Int] = unfold(1)(_ => Some(1, 1))

  def from_1(n: Int): Stream[Int] = unfold(n)(s => Some(s, s + 1))
}