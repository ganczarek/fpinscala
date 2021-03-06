package fpinscala
package monads

import fpinscala.testing._

import scala.concurrent.{ExecutionContext, Future}
import scala.language.higherKinds


trait Functor[F[_]] {
  def map[A, B](fa: F[A])(f: A => B): F[B]

  def distribute[A, B](fab: F[(A, B)]): (F[A], F[B]) =
    (map(fab)(_._1), map(fab)(_._2))

  def codistribute[A, B](e: Either[F[A], F[B]]): F[Either[A, B]] = e match {
    case Left(fa) => map(fa)(Left(_))
    case Right(fb) => map(fb)(Right(_))
  }
}

object Functor {
  val listFunctor = new Functor[List] {
    def map[A, B](as: List[A])(f: A => B): List[B] = as map f
  }
}

trait Monad[M[_]] extends Functor[M] {
  def unit[A](a: => A): M[A]

  def flatMap[A, B](ma: M[A])(f: A => M[B]): M[B]

  def map[A, B](ma: M[A])(f: A => B): M[B] =
    flatMap(ma)(a => unit(f(a)))

  def map2[A, B, C](ma: M[A], mb: M[B])(f: (A, B) => C): M[C] =
    flatMap(ma)(a => map(mb)(b => f(a, b)))

  def sequence[A](lma: List[M[A]]): M[List[A]] =
    lma.foldRight(unit(List[A]()))((monadA, mListA) => map2(monadA, mListA)(_ :: _))

  def traverse[A, B](la: List[A])(f: A => M[B]): M[List[B]] =
    la.foldRight(unit(List[B]()))((elemA, mListOfB) => map2(f(elemA), mListOfB)(_ :: _))

  def replicateM[A](n: Int, ma: M[A]): M[List[A]] = traverse((1 to n).toList)(x => ma)

  def filterM[A](ms: List[A])(f: A => M[Boolean]): M[List[A]] = ms match {
    case Nil => unit(Nil)
    case head :: tail => flatMap(f(head)) {
      // keep the head and append to the filterM result of the tail
      case true => map(filterM(tail)(f))(head :: _)
      // filter out the head, continue filtering the rest of the list
      case false => filterM(tail)(f)
    }
  }

  def compose[A, B, C](f: A => M[B], g: B => M[C]): A => M[C] = a => flatMap(f(a))(g)

  // Implement in terms of `compose`:
  def _flatMap[A, B](ma: M[A])(f: A => M[B]): M[B] = compose[Unit, A, B](_ => ma, f)(Unit)

  def join[A](mma: M[M[A]]): M[A] = flatMap(mma)(Predef.identity)

  // Implement in terms of `join`:
  def __flatMap[A, B](ma: M[A])(f: A => M[B]): M[B] = ???
}

case class Reader[R, A](run: R => A)

object Monad {
  val genMonad = new Monad[Gen] {
    def unit[A](a: => A): Gen[A] = Gen.unit(a)

    override def flatMap[A, B](ma: Gen[A])(f: A => Gen[B]): Gen[B] =
      ma flatMap f
  }

  val optionMonad: Monad[Option] = new Monad[Option] {
    override def flatMap[A, B](ma: Option[A])(f: (A) => Option[B]): Option[B] = ma flatMap f

    override def unit[A](a: => A): Option[A] = Option(a)
  }

  val streamMonad: Monad[Stream] = new Monad[Stream] {
    override def flatMap[A, B](ma: Stream[A])(f: (A) => Stream[B]): Stream[B] = ma flatMap f

    override def unit[A](a: => A): Stream[A] = Stream(a)
  }

  val listMonad: Monad[List] = new Monad[List] {
    override def flatMap[A, B](ma: List[A])(f: (A) => List[B]): List[B] = ma flatMap f

    override def unit[A](a: => A): List[A] = List(a)
  }

  val futureMonda: Monad[Future] = new Monad[Future] {
    implicit val ec = ExecutionContext.global

    override def flatMap[A, B](ma: Future[A])(f: (A) => Future[B]): Future[B] = ma flatMap f

    override def unit[A](a: => A): Future[A] = Future(a)
  }

  //  val parMonad: Monad[Par] = ???
  //  def parserMonad[P[+_]](p: Parsers[P]): Monad[P] = ???

  //  def stateMonad[S] = ???
  //  val idMonad: Monad[Id] = ???
  //  def readerMonad[R] = ???
}

case class Id[A](value: A) {
  def map[B](f: A => B): Id[B] = ???

  def flatMap[B](f: A => Id[B]): Id[B] = ???
}

object Reader {
  def readerMonad[R] = new Monad[({type f[x] = Reader[R, x]})#f] {
    def unit[A](a: => A): Reader[R, A] = ???

    override def flatMap[A, B](st: Reader[R, A])(f: A => Reader[R, B]): Reader[R, B] = ???
  }
}

