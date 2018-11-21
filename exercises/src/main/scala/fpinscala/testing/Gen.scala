package fpinscala.testing

import fpinscala.state._

/*
The library developed in this chapter goes through several iterations. This file is just the
shell, which you can fill in and modify while working through the chapter.
*/

trait Prop {
}

object Prop {
  def forAll[A](gen: Gen[A])(f: A => Boolean): Prop = ???
}

case class Gen[+A](sample: State[RNG, A]) {
  def map[B](f: A => B): Gen[B] = Gen(sample map f)

  def flatMap[B](f: A => Gen[B]): Gen[B] = Gen(sample.flatMap(x => f(x).sample))

  def listOfN(size: Gen[Int]): Gen[List[A]] = size flatMap (Gen.listOfN(_, this))
}

object Gen {
  def choose(start: Int, stopExclusive: Int): Gen[Int] =
    Gen(State(RNG.nonNegativeInt)).map(n => start + n % (stopExclusive - start))

  def unit[A](a: => A): Gen[A] = Gen(State.unit(a))

  def boolean: Gen[Boolean] = Gen(State(RNG.int)).map(_ % 2 == 0)

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] = Gen(State.sequence(List.fill(n)(g.sample)))
}

trait SGen[+A] {

}

