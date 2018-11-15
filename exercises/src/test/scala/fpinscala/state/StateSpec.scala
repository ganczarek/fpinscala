package fpinscala.state

import fpinscala.state.RNG.{Rand, Simple}
import org.scalatest.{FlatSpec, Matchers}

class StateSpec extends FlatSpec with Matchers {

  case class TestRNG(value: Int, nextRng: RNG = Simple(-999)) extends RNG {
    override def nextInt: (Int, RNG) = (value, nextRng)
  }

  def buildTestRNG(l: List[Int]): RNG = l.foldRight(Simple(-999): RNG)((i, acc) => TestRNG(i, acc))

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
    RNG.intDouble(buildTestRNG(List(0, 0))) shouldBe((0, 0.0), Simple(-999))
    RNG.intDouble(buildTestRNG(List(-10, 0))) shouldBe((-10, 0.0), Simple(-999))
  }

  "RNG.doubleInt" should "return (Double,Int) tuple" in {
    RNG.doubleInt(buildTestRNG(List(0, 0))) shouldBe((0.0, 0.0), Simple(-999))
    RNG.doubleInt(buildTestRNG(List(-10, 0))) shouldBe((0.0, -10), Simple(-999))
  }

  "RNG.double3" should "use different RNG in each step" in {
    RNG.double3(buildTestRNG(List(0, 0, 0))) shouldBe((0.0, 0.0, 0.0), Simple(-999))
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

  behavior of "Exercise 6.4"

  "RNG.ints" should "return empty list and the same RNG for 0 elements" in {
    RNG.ints(0)(Simple(10)) shouldBe(List(), Simple(10))
  }

  "RNG.ints" should "generate list of random integers" in {
    val result = RNG.ints(5)(Simple(0))

    result._1.size shouldBe 5
    result._2 shouldBe a[RNG]
  }

  "RNG.ints" should "use different RNG for each step" in {
    RNG.ints(3)(buildTestRNG(List(1, 2, 3, 4))) shouldBe(List(1, 2, 3), TestRNG(4))
  }

  behavior of "Exercise 6.5"

  "RNG.doubleWithMap()" should "return a double between 0 and 1" in {
    RNG.doubleWithMap(Simple(10)) shouldBe(3847489.toDouble / (Int.MaxValue.toDouble + 1), Simple(252149039181L))
  }

  "RNG.doubleWithMap()" should "should return double value between 0 and 1 for all edge cases" in {
    val testCases = List(
      (0, 0),
      (-1, 0),
      (1, 1 / (Int.MaxValue.toDouble + 1)),
      (-2, 1 / (Int.MaxValue.toDouble + 1)),
      (Int.MinValue, Int.MaxValue / (Int.MaxValue.toDouble + 1))
    )

    testCases foreach {
      case (testValue, expectedValue) => RNG.doubleWithMap(TestRNG(testValue)) shouldBe(expectedValue, Simple(-999))
    }
  }

  behavior of "Exercise 6.6"

  "RNG.map2" should "combine results according to provided function" in {
    RNG.map2(_.nextInt, _.nextInt)(_ + _)(buildTestRNG(List(10, 12))) shouldBe(22, Simple(-999))
  }

  behavior of "Exercise 6.7"

  "RNG.sequence" should "combine whole list of RNG transitions" in {
    val listOfRands: List[Rand[Int]] = List(_.nextInt, rng => (10, rng), _.nextInt)
    RNG.sequence(listOfRands)(buildTestRNG(List(1, 2))) shouldBe(List(1, 10, 2), Simple(-999))
  }

  "RNG.intsWithSequence" should "generate list of random integers" in {
    val result = RNG.intsWithSequence(5)(Simple(0))

    result._1.size shouldBe 5
    result._2 shouldBe a[RNG]
  }

  behavior of "Exercise 6.8"

  "RNG.flatMap" should "map and flatten" in {
    RNG.flatMap(_.nextInt)(x => rng => RNG.ints(x)(rng))(buildTestRNG(List(2, 1, 2))) shouldBe(List(1, 2), Simple(-999))
  }

  "RNG.nonNegativeLessThan" should "return first value being less than given value" in {
    RNG.nonNegativeLessThan(1)(buildTestRNG(List(10, 5, 0, 1))) shouldBe(0, TestRNG(1))
  }

  behavior of "Exercise 6.9"

  "RNG.mapWithFlatMap" should "map" in {
    RNG.mapWithFlatMap(_.nextInt)(_.toString)(buildTestRNG(List(1, 2))) shouldBe("1", TestRNG(2))
  }

  "RNG.map2WithFlatMap" should "combine results according to provided function" in {
    RNG.map2WithFlatMap(_.nextInt, _.nextInt)(_ + _)(buildTestRNG(List(10, 12))) shouldBe(22, Simple(-999))
  }

  behavior of "Exercise 6.10"

  "State.unit" should "should return " in {
    State.unit(1).run("nextState") shouldBe(1, "nextState")
  }

  "State.map" should "map from one state to another" in {
    State.unit(1).map(_.toString).run("nextState") shouldBe ("1", "nextState")
  }

  "State.map2" should "map two states to another" in {
    val stateA: State[String, Int] = State.unit(1)
    val stateB: State[String, String] = State(s => (" " + s, "state after " + s))

    stateA.map2(stateB)((i, s) => i.toString + s).run("start") shouldBe ("1 start", "state after start")
  }

  "State.flatMap" should "map and flatten" in {
    val stateA: State[String, Int] = State.unit(1)
    stateA.flatMap(i => State.unit(i*10)).run("nextState") shouldBe (10, "nextState")
  }
}
