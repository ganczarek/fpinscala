package fpinscala.testing

import fpinscala.laziness.Stream
import fpinscala.state._
import fpinscala.testing.Prop._

object Prop {
  type FailedCase = String
  type SuccessCount = Int
  type TestCases = Int
  type MaxSize = Int

  sealed trait Result {
    def isFalsified: Boolean
  }

  case object Passed extends Result {
    override def isFalsified: Boolean = false
  }

  case class Falsified(failure: FailedCase, successes: SuccessCount) extends Result {
    override def isFalsified: Boolean = true
  }

  def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop {
    (n, rng) =>
      randomStream(as)(rng).zip(Stream.from(0)).take(n).map {
        case (a, i) => try {
          if (f(a)) Passed else Falsified(a.toString, i)
        } catch {
          case e: Exception => Falsified(buildMsg(a, e), i)
        }
      }.find(_.isFalsified).getOrElse(Passed)
  }

  def apply(f: (TestCases, RNG) => Result): Prop = Prop { (_, n, rng) => f(n, rng) }

  def forAll[A](g: SGen[A])(f: A => Boolean): Prop =
    forAll(g(_))(f)

  def forAll[A](g: Int => Gen[A])(f: A => Boolean): Prop = Prop {
    (max, n, rng) =>
      val casesPerSize = (n - 1) / max + 1
      val props: Stream[Prop] =
        Stream.from(0).take((n min max) + 1).map(i => forAll(g(i))(f))
      val prop: Prop =
        props.map(p => Prop { (max, _, rng) =>
          p.run(max, casesPerSize, rng)
        }).toList.reduce(_ && _)
      prop.run(max, n, rng)
  }

  def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] =
    Stream.unfold(rng)(rng => Some(g.sample.run(rng)))

  def buildMsg[A](s: A, e: Exception): String =
    s"test case: $s\n" +
      s"generated an exception: ${e.getMessage}\n" +
      s"stack trace:\n ${e.getStackTrace.mkString("\n")}"

}

case class Prop(run: (MaxSize, TestCases, RNG) => Result) {

  def &&(prop: Prop): Prop = Prop {
    (max, n, rng) =>
      run(max, n, rng) match {
        case Passed => prop.run(max, n, rng)
        case x => x
      }
  }

  def ||(prop: Prop): Prop = Prop {
    (max, n, rng) =>
      run(max, n, rng) match {
        case x: Falsified => prop.previousFailure(x).run(max, n, rng)
        case x => x
      }
  }

  def previousFailure(f: Falsified): Prop = Prop {
    (max, n, rng) =>
      run(max, n, rng) match {
        case Falsified(msg, successCount) => Falsified(f.failure + "\n" + msg, successCount + f.successes)
        case x => x
      }
  }

}

case class Gen[+A](sample: State[RNG, A]) {
  def map[B](f: A => B): Gen[B] = Gen(sample map f)

  def flatMap[B](f: A => Gen[B]): Gen[B] = Gen(sample.flatMap(x => f(x).sample))

  def listOfN(size: Gen[Int]): Gen[List[A]] = size flatMap (Gen.listOfN(_, this))

  def unsized: SGen[A] = SGen(_ => this)
}

object Gen {
  def choose(start: Int, stopExclusive: Int): Gen[Int] =
    Gen(State(RNG.nonNegativeInt)).map(n => start + n % (stopExclusive - start))

  def unit[A](a: => A): Gen[A] = Gen(State.unit(a))

  def boolean: Gen[Boolean] = Gen(State(RNG.int)).map(_ % 2 == 0)

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] = Gen(State.sequence(List.fill(n)(g.sample)))

  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] = boolean flatMap (if (_) g1 else g2)

  def uniform: Gen[Double] = Gen(State(RNG.double))

  def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] =
    uniform flatMap (d => if (d < g1._2) g1._1 else g2._1)

  def listOf[A](g: Gen[A]): SGen[List[A]] = SGen(n => g.listOfN(Gen.unit(n)))
}

case class SGen[+A](forSize: Int => Gen[A]) {
  def apply(n: Int): Gen[A] = forSize(n)

  def map[B](f: A => B): SGen[B] = flatMap(a => SGen.unit(f(a)))

  def flatMap[B](f: A => SGen[B]): SGen[B] = SGen {
    x => forSize(x).flatMap(f(_).forSize(x))
  }
}

object SGen {
  def unit[A](a: => A): SGen[A] = Gen.unit(a).unsized
}

