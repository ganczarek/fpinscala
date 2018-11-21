package fpinscala.testing

import fpinscala.state.RNG.Simple
import org.scalatest.{FlatSpec, Matchers}

class GenSpec extends FlatSpec with Matchers {

  behavior of "Exercise 8.4"

  "Gen.choose" should "should generate integer numbers from [a, b) interval" in {
    Gen.choose(0, 1).sample.run(Simple(10))._1 shouldBe 0
    Gen.choose(1, 2).sample.run(Simple(10))._1 shouldBe 1
    Gen.choose(0, 5).sample.run(Simple(10))._1 should be < 5
    Gen.choose(1, 5).sample.run(Simple(10))._1 should be >= 1
  }

}
