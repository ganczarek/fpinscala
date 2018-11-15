package fpinscala.state

import fpinscala.state.RNG.Simple
import org.scalatest.{FlatSpec, Matchers}

class StateSpec extends FlatSpec with Matchers {

  case class TestRNG(value: Int) extends RNG {
    override def nextInt: (Int, RNG) = (value, Simple(-999))
  }

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
      case (testValue, expectedValue) => RNG.nonNegativeInt(TestRNG(testValue)) shouldBe(expectedValue, Simple(-999))
    }
  }

  behavior of "Exercise 6.2"

  "RNG.double" should "return a double between 0 and 1" in {
    RNG.double(Simple(10)) shouldBe(3847489.toDouble / (Int.MaxValue.toDouble + 1), Simple(252149039181L))
  }

  "RNG.double" should "should return double value between 0 and 1 for all edge cases" in {
    val testCases = List(
      (0, 0),
      (-1, 0),
      (1, 1 / (Int.MaxValue.toDouble + 1)),
      (-2, 1 / (Int.MaxValue.toDouble + 1)),
      (Int.MinValue, Int.MaxValue / (Int.MaxValue.toDouble + 1))
    )

    testCases foreach {
      case (testValue, expectedValue) => RNG.double(TestRNG(testValue)) shouldBe(expectedValue, Simple(-999))
    }
  }
}
