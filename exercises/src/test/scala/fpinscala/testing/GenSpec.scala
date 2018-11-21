package fpinscala.testing

import fpinscala.state.RNG
import fpinscala.state.RNG.Simple
import org.scalatest.{FlatSpec, Matchers}

class GenSpec extends FlatSpec with Matchers {

  case class TestRNG(value: Int, nextRng: RNG = Simple(-999)) extends RNG {
    override def nextInt: (Int, RNG) = (value, nextRng)
  }

  val rng = TestRNG(10, TestRNG(11))

  behavior of "Exercise 8.4"

  "Gen.choose" should "generate integer numbers from [a, b) interval" in {
    Gen.choose(0, 1).sample.run(rng)._1 shouldBe 0
    Gen.choose(1, 2).sample.run(rng)._1 shouldBe 1
    Gen.choose(0, 5).sample.run(rng)._1 should be < 5
    Gen.choose(1, 5).sample.run(rng)._1 should be >= 1
  }

  behavior of "Exercise 8.5"

  "Gen.unit" should "always generate the same value" in {
    Gen.unit(5).sample.run(rng)._1 shouldBe 5
  }

  "Gen.boolean" should "generate boolean values" in {
    Gen.boolean.sample.run(rng)._1 shouldBe true // 10 % 2 == 0
  }

  "Gen.listOfN" should "generate empty list when 0 elements requested" in {
    Gen.listOfN(0, Gen.boolean).sample.run(rng)._1 shouldBe List()
  }

  "Gen.listOfN" should "generate a list of length 1 when 1 element requested" in {
    Gen.listOfN(1, Gen.boolean).sample.run(rng)._1 shouldBe List(true)
    Gen.listOfN(1, Gen.choose(0, 1)).sample.run(rng)._1 shouldBe List(0)
  }

  "Gen.listOfN" should "generate lists of length n using given generator" in {
    Gen.listOfN(5, Gen.unit(5)).sample.run(rng)._1 shouldBe List(5, 5, 5, 5, 5)
    Gen.listOfN(2, Gen.boolean).sample.run(rng)._1 shouldBe List(true, false)
  }

  behavior of "Exercise 8.6"

  "Gen.flatMap" should "flatten generator of generators" in {
    Gen.unit(5).flatMap(_ => Gen.boolean).sample.run(rng)._1 shouldBe true
    Gen.unit(5).flatMap(x => Gen.unit(x + 1)).sample.run(rng)._1 shouldBe 6
  }

  "Gen.listOfN" should "give a generator of lists with values from current generator" in {
    Gen.unit(5).listOfN(Gen.unit(3)).sample.run(rng)._1 shouldBe List(5, 5, 5)
    Gen.boolean.listOfN(Gen.unit(2)).sample.run(rng)._1 shouldBe List(true, false)
  }
}
