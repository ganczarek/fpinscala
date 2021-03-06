package fpinscala.state

import fpinscala.state.RNG.{Rand, map2, unit}


trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

object RNG {

  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def nonNegativeInt(rng: RNG): (Int, RNG) = rng.nextInt match {
    case (i, rng2) if i < 0 => (-(i + 1), rng2)
    case x => x
  }

  def double(rng: RNG): (Double, RNG) = {
    val (i, rng2) = RNG.nonNegativeInt(rng)
    (i / (Int.MaxValue.toDouble + 1), rng2)
  }

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, rng2) = rng.nextInt
    val (d, rng3) = RNG.double(rng2)
    ((i, d), rng3)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val ((i, d), rng2) = RNG.intDouble(rng)
    ((d, i), rng2)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, rng2) = RNG.double(rng)
    val (d2, rng3) = RNG.double(rng2)
    val (d3, rng4) = RNG.double(rng3)
    ((d1, d2, d3), rng4)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    if (count == 0)
      return (List(), rng)

    val (i, rng2) = rng.nextInt
    val (l, rng3) = RNG.ints(count - 1)(rng2)
    (i :: l, rng3)
  }

  def doubleWithMap(rng: RNG): (Double, RNG) = map(nonNegativeInt)(_ / (Int.MaxValue.toDouble + 1))(rng)

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = rng => {
    val (a, rng2) = ra(rng)
    val (b, rng3) = rb(rng2)
    (f(a, b), rng3)
  }

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight(unit(List[A]()))((rand, acc) => map2(rand, acc)(_ :: _))

  def intsWithSequence(count: Int): Rand[List[Int]] = sequence(List.fill(count)(_.nextInt))

  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = rng => {
    val (a, rng2) = f(rng)
    g(a)(rng2)
  }

  def nonNegativeLessThan(n: Int): Rand[Int] = flatMap(nonNegativeInt)(i =>
    if(i < n) unit(i) else nonNegativeLessThan(n)
  )

  def mapWithFlatMap[A, B](s: Rand[A])(f: A => B): Rand[B] = flatMap(s)(a => unit(f(a)))

  def map2WithFlatMap[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra)(a => mapWithFlatMap(rb)(b => f(a, b)))
}

case class State[S,+A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] = flatMap(a => State.unit(f(a)))

  def map_2[B](f: A => B): State[S, B] = State(s => {
    val (a, nextState) = run(s)
    (f(a), nextState)
  })

  def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] = flatMap(a => sb.map(b => f(a, b)))

  def map2_2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] = State(s => {
    val (a, s1) = run(s)
    val (b, s2) = sb.run(s1)
    (f(a,b), s2)
  })

  def flatMap[B](f: A => State[S, B]): State[S, B] = State(s => {
    val (a, s2) = run(s)
    f(a).run(s2)
  })
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object State {
  type Rand[A] = State[RNG, A]

  def unit[S, A](a: A): State[S, A] = State(s => (a, s))

  def sequence[S, A](fs: List[State[S, A]]): State[S, List[A]] =
    fs.foldRight(unit[S, List[A]](List()))((f, acc) => f.map2(acc)(_ :: _))

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = ???
}
