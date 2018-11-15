package fpinscala.state

import fpinscala.state.RNG.Simple
import org.scalatest.{FlatSpec, Matchers}

class StateSpec extends FlatSpec with Matchers {

  behavior of "Exercise 6.1"

  "RNG.nonNegativeInt" should "return negative int" in {
    RNG.nonNegativeInt(Simple(10)) shouldBe(3847489, Simple(252149039181L))
  }

  "RNG.nonNegativeInt" should "return positive values for edge cases" in {
    val testCases = List(
      (0, 0),
      (1, 1),
      (-1, 0),
      (-2, 1),
      (Int.MinValue, Int.MaxValue)
    )

    testCases foreach {
      case (testValue, expectedValue) =>
        case class TestRNG() extends RNG {
          override def nextInt: (Int, RNG) = (testValue, Simple(10))
        }
        RNG.nonNegativeInt(TestRNG()) shouldBe(expectedValue, Simple(10))
    }
  }

}
