package fpinscala.testing

import fpinscala.state.RNG
import fpinscala.state.RNG.Simple
import fpinscala.testing.Prop.{Falsified, Passed}
import org.scalatest.{FlatSpec, Matchers}

class GenSpec extends FlatSpec with Matchers {

  case class TestRNG(value: Int, nextRng: RNG = Simple(-999)) extends RNG {
    override def nextInt: (Int, RNG) = (value, nextRng)
  }

  val rng = TestRNG(10, TestRNG(11, TestRNG(12)))

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

  behavior of "Exercise 8.7"

  "Gen.union" should "combine two generators and pull values from each one with equal likelihood" in {
    Gen.union(Gen.unit(5), Gen.unit(3)).sample.run(rng)._1 shouldBe 5
    Gen.union(Gen.unit(5), Gen.unit(3)).listOfN(Gen.unit(3)).sample.run(rng)._1 shouldBe List(5, 3, 5)
  }

  behavior of "Exercise 8.8"

  "Gen.uniform" should "generate values from uniform distribution" in {
    Gen.uniform.sample.run(rng)._1 shouldBe 10 / (Int.MaxValue.toDouble + 1)
  }

  "Gen.weighted" should "" in {
    // because of `rng` outputting low values (10, 11 and 12), first generator is selected as long as value is < 12
    val p1 = 12 / (Int.MaxValue.toDouble + 1)
    Gen.weighted((Gen.unit(1), p1), (Gen.unit(2), 1 - p1)).listOfN(Gen.unit(3)).sample.run(rng)._1 shouldBe List(1, 1, 2)
  }

  behavior of "Exercise 8.9"

  "Prop.&&" should "fail when any of props fails" in {
    val g = Gen.choose(0, 1000)
    val trueForAll: Int => Boolean = _ < 1000
    val falseForAll: Int => Boolean = _ < 0

    Prop.forAll(g)(trueForAll).run(1000, 1000, Simple(1000)) shouldBe Passed
    Prop.forAll(g)(falseForAll).run(1000, 1000, Simple(1000)) shouldBe a[Falsified]

    (Prop.forAll(g)(trueForAll) && Prop.forAll(g)(trueForAll)).run(1000, 1000, Simple(1000)).isFalsified shouldBe false
    (Prop.forAll(g)(falseForAll) && Prop.forAll(g)(falseForAll)).run(1000, 1000, Simple(1000)).isFalsified shouldBe true
    (Prop.forAll(g)(trueForAll) && Prop.forAll(g)(falseForAll)).run(1000, 1000, Simple(1000)).isFalsified shouldBe true
    (Prop.forAll(g)(falseForAll) && Prop.forAll(g)(trueForAll)).run(1000, 1000, Simple(1000)).isFalsified shouldBe true
  }

  "Prop.||" should "fail only when both props fail" in {
    val g = Gen.choose(0, 1000)
    val trueForAll: Int => Boolean = _ < 1000
    val falseForAll: Int => Boolean = _ < 0


    Prop.forAll(g)(trueForAll).run(1000, 1000, Simple(1000)) shouldBe Passed
    Prop.forAll(g)(falseForAll).run(1000, 1000, Simple(1000)) shouldBe a[Falsified]

    (Prop.forAll(g)(trueForAll) || Prop.forAll(g)(falseForAll)).run(1000, 1000, Simple(1000)).isFalsified shouldBe false
    (Prop.forAll(g)(falseForAll) || Prop.forAll(g)(falseForAll)).run(1000, 1000, Simple(1000)).isFalsified shouldBe true
    (Prop.forAll(g)(trueForAll) || Prop.forAll(g)(falseForAll)).run(1000, 1000, Simple(1000)).isFalsified shouldBe false
    (Prop.forAll(g)(falseForAll) || Prop.forAll(g)(trueForAll)).run(1000, 1000, Simple(1000)).isFalsified shouldBe false
  }

  behavior of "Exercise 8.10"

  "Gen.unsized" should "return SGen" in {
    Gen.unit(5).unsized shouldBe a[SGen[_]]
  }

  behavior of "Exercise 8.12"

  "Gen.listOf" should "should generate lists of requested size" in {
    List(100, 10, 5, 1, 0) foreach { n =>
      Gen.listOf(Gen.unit(1))(n).sample.run(rng)._1 shouldBe List.fill(n)(1)
    }
  }

  behavior of "Exercise 8.13"

  "Prop.run" should "help to run tests" in {
    val smallInt = Gen.choose(-10,10)
    val maxProp = Prop.forAll(Gen.listOf1(smallInt)) { ns =>
      val max = ns.max
      !ns.exists(_ > max)
    }
    Prop.run(maxProp) shouldBe Passed
  }

  "Prop.listOf1" should "should generate non-empty lists" in {
    Gen.listOf1(Gen.unit(1))(0).sample.run(rng)._1 shouldBe List(1)
  }

}
