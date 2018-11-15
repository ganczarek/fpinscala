package fpinscala.state

import fpinscala.state.RNG.Simple
import org.scalatest.{FlatSpec, Matchers}

class StateSpec extends FlatSpec with Matchers {

  case class TestRNG(value: Int, nextRng: RNG = Simple(-999)) extends RNG {
    override def nextInt: (Int, RNG) = (value, nextRng)
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

  behavior of "Exercise 6.3"

  "RNG.intDouble" should "return (Int,Double) tuple" in {
    RNG.intDouble(TestRNG(0, TestRNG(0))) shouldBe((0, 0.0), Simple(-999))
    RNG.intDouble(TestRNG(-10, TestRNG(-1))) shouldBe((-10, 0.0), Simple(-999))
  }

  "RNG.doubleInt" should "return (Double,Int) tuple" in {
    RNG.doubleInt(TestRNG(0, TestRNG(0))) shouldBe((0.0, 0.0), Simple(-999))
    RNG.doubleInt(TestRNG(-10, TestRNG(-1))) shouldBe((0.0, -10), Simple(-999))
  }

  "RNG.double3" should "use different RNG in each step" in {
    RNG.double3(TestRNG(0, TestRNG(0, TestRNG(0)))) shouldBe((0.0, 0.0, 0.0), Simple(-999))
  }

  "RNG.double3" should "return three doubles" in {
    RNG.double3(Simple(10)) match {
      case ((d1, d2, d3), rng) =>
        d1 shouldBe a[java.lang.Double]
        d1 >= 0 shouldBe true
        d1 < 1 shouldBe true

        d2 shouldBe a[java.lang.Double]
        d2 >= 0 shouldBe true
        d2 < 1 shouldBe true

        d3 shouldBe a[java.lang.Double]
        d3 >= 0 shouldBe true
        d3 < 1 shouldBe true

        rng shouldBe a[RNG]
    }
  }
}
