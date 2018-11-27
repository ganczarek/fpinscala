package fpinscala.iomonad

import org.scalatest.{FlatSpec, Matchers}

class IOSpec extends FlatSpec with Matchers {

  behavior of "Exercise 13.1"

  "IO3.freeMonad" should "return monad for TailRec" in {
    val freeMonad = IO3.freeMonad

    freeMonad.unit(10) shouldBe Return(10)
  }

  behavior of "Exercise 13.2"

  "IO3.runTrampoline" should "return value" in {
    IO3.runTrampoline(IO3.Return(10)) shouldBe 10
  }

  "IO3.runTrampoline" should "return suspend computation" in {
    IO3.runTrampoline(IO3.Suspend(() => 10)) shouldBe 10
  }

  "IO3.runTrampoline" should "flat map value" in {
    IO3.runTrampoline(IO3.FlatMap(IO3.Return(10), (x: Int) => IO3.Return(x * 2))) shouldBe 20
    IO3.runTrampoline(IO3.FlatMap(IO3.Return(10), (x: Int) => IO3.Suspend(() => x * 2))) shouldBe 20
    IO3.runTrampoline(IO3.FlatMap(IO3.Suspend(() => 10), (x: Int) => IO3.Suspend(() => x * 2))) shouldBe 20
    IO3.runTrampoline(IO3.FlatMap(IO3.Suspend(() => 10), (x: Int) => IO3.Return(x * 2))) shouldBe 20
  }

  behavior of "Exercise 13.3"

  "IO3.run" should "work for any freeMonad" in {
    // how to use IO3.freeMonad ?
    val function0Monad = new Monad[Function0] {
      override def unit[A](a: => A): () => A = () => a

      override def flatMap[A, B](a: () => A)(f: A => () => B): () => B = f(a())
    }

    type TailRec[A] = Free[Function0, A]
    val testCases: Seq[TailRec[Int]] = Seq(
      IO3.Suspend(() => 10),
      IO3.Return(10),
      IO3.FlatMap(IO3.Suspend(() => 5), (x: Int) => IO3.Return(x * 2))
    )
    testCases foreach { testCase =>
      IO3.run(testCase)(function0Monad)() shouldBe 10
    }
  }
}