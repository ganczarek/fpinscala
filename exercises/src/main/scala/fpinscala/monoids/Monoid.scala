package fpinscala.monoids

// infix syntax for `Par.map`, `Par.flatMap`, etc
import scala.concurrent.{ExecutionContext, Future}
import scala.language.higherKinds

trait Monoid[A] {
  def op(a1: A, a2: A): A

  def zero: A
}

object Monoid {

  val stringMonoid = new Monoid[String] {
    def op(a1: String, a2: String) = a1 + a2

    val zero = ""
  }

  def listMonoid[A] = new Monoid[List[A]] {
    def op(a1: List[A], a2: List[A]) = a1 ++ a2

    val zero = Nil
  }

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

  def endoMonoid[A]: Monoid[A => A] = new Monoid[(A) => A] {
    override def op(f: (A) => A, g: (A) => A): (A) => A = f andThen g

    override def zero: (A) => A = Predef.identity
  }

  def flipEndoMonoid[A]: Monoid[A => A] = new Monoid[(A) => A] {
    override def op(f: (A) => A, g: (A) => A): (A) => A = g andThen f

    override def zero: (A) => A = Predef.identity
  }

  // For monoidLaws see ScalaCheck properties in MonoidSpecification.scala
  //def monoidLaws[A](m: Monoid[A], gen: Gen[A]): Prop = ???

  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B = as.foldLeft(m.zero)((x: B, y: A) => m.op(x, f(y)))

  def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B =
    Monoid.foldMap(as, Monoid.endoMonoid[B])(a => b => f(b, a))(z)

  // andThen operation is not commutative, flip it (create dual of endoMonoid)
  def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B =
  Monoid.foldMap(as, Monoid.flipEndoMonoid[B])(f.curried)(z)

  def foldMapV[A, B](as: IndexedSeq[A], m: Monoid[B])(f: A => B): B = {
    as match {
      case IndexedSeq() => m.zero
      case IndexedSeq(x) => f(x)
      case _ =>
        val (left, right) = as.splitAt(as.size / 2)
        m.op(Monoid.foldMapV(left, m)(f), Monoid.foldMapV(right, m)(f))
    }
  }

  def par[A](m: Monoid[A]): Monoid[Future[A]] = new Monoid[Future[A]] {
    implicit val ec = ExecutionContext.global

    override def op(a1: Future[A], a2: Future[A]): Future[A] = for {
      value1 <- a1
      value2 <- a2
    } yield m.op(value1, value2)

    override def zero: Future[A] = Future(m.zero)
  }

  def parFoldMap[A, B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): Future[B] = {
    implicit val ec = ExecutionContext.global
    foldMapV(v, par(m))((value: A) => Future {
      f(value)
    })
  }

  def ordered(ints: IndexedSeq[Int]): Boolean = {
    type ValueTrack = (Int, Int, Boolean, Boolean)
    val trackingMonoid = new Monoid[Option[ValueTrack]] {
      override def op(a1: Option[ValueTrack], a2: Option[ValueTrack]): Option[ValueTrack] = {
        (a1, a2) match {
          case (Some((min1, max1, isAscending1, isDescending1)), Some((min2, max2, isAscending2, isDescending2))) =>
            val isAscending = isAscending1 && isAscending2 && max1 <= max2
            val isDescending = isDescending1 && isDescending2 && min1 >= min2
            Some(min1 min min2, max1 max max2, isAscending, isDescending)
          case (None, a) => a
          case (a, None) => a
        }
      }

      override def zero: Option[ValueTrack] = None
    }
    foldMapV(ints, trackingMonoid)(i => Some(i, i, true, true)).forall(x => x._3 || x._4)
  }


  /*
  def trimMonoid(s: String): Monoid[String] = sys.error("todo")

  def concatenate[A](as: List[A], m: Monoid[A]): A =
    sys.error("todo")



  def ordered(ints: IndexedSeq[Int]): Boolean =
    sys.error("todo")

  sealed trait WC

  case class Stub(chars: String) extends WC

  case class Part(lStub: String, words: Int, rStub: String) extends WC

  val wcMonoid: Monoid[WC] = sys.error("todo")

  def count(s: String): Int = sys.error("todo")

  def productMonoid[A, B](A: Monoid[A], B: Monoid[B]): Monoid[(A, B)] =
    sys.error("todo")

  def functionMonoid[A, B](B: Monoid[B]): Monoid[A => B] =
    sys.error("todo")

  def mapMergeMonoid[K, V](V: Monoid[V]): Monoid[Map[K, V]] =
    sys.error("todo")

  def bag[A](as: IndexedSeq[A]): Map[A, Int] =
    sys.error("todo")

  */
}

trait Foldable[F[_]] {

  def foldRight[A, B](as: F[A])(z: B)(f: (A, B) => B): B =
    sys.error("todo")

  def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) => B): B =
    sys.error("todo")

  def foldMap[A, B](as: F[A])(f: A => B)(mb: Monoid[B]): B =
    sys.error("todo")

  def concatenate[A](as: F[A])(m: Monoid[A]): A =
    sys.error("todo")

  def toList[A](as: F[A]): List[A] =
    sys.error("todo")
}

object ListFoldable extends Foldable[List] {
  override def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B) =
    sys.error("todo")

  override def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B) =
    sys.error("todo")

  override def foldMap[A, B](as: List[A])(f: A => B)(mb: Monoid[B]): B =
    sys.error("todo")
}

object IndexedSeqFoldable extends Foldable[IndexedSeq] {
  override def foldRight[A, B](as: IndexedSeq[A])(z: B)(f: (A, B) => B) =
    sys.error("todo")

  override def foldLeft[A, B](as: IndexedSeq[A])(z: B)(f: (B, A) => B) =
    sys.error("todo")

  override def foldMap[A, B](as: IndexedSeq[A])(f: A => B)(mb: Monoid[B]): B =
    sys.error("todo")
}

object StreamFoldable extends Foldable[Stream] {
  override def foldRight[A, B](as: Stream[A])(z: B)(f: (A, B) => B) =
    sys.error("todo")

  override def foldLeft[A, B](as: Stream[A])(z: B)(f: (B, A) => B) =
    sys.error("todo")
}

sealed trait Tree[+A]

case class Leaf[A](value: A) extends Tree[A]

case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object TreeFoldable extends Foldable[Tree] {
  override def foldMap[A, B](as: Tree[A])(f: A => B)(mb: Monoid[B]): B =
    sys.error("todo")

  override def foldLeft[A, B](as: Tree[A])(z: B)(f: (B, A) => B) =
    sys.error("todo")

  override def foldRight[A, B](as: Tree[A])(z: B)(f: (A, B) => B) =
    sys.error("todo")
}

object OptionFoldable extends Foldable[Option] {
  override def foldMap[A, B](as: Option[A])(f: A => B)(mb: Monoid[B]): B =
    sys.error("todo")

  override def foldLeft[A, B](as: Option[A])(z: B)(f: (B, A) => B) =
    sys.error("todo")

  override def foldRight[A, B](as: Option[A])(z: B)(f: (A, B) => B) =
    sys.error("todo")
}

