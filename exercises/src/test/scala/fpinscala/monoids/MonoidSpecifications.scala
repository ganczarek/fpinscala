package fpinscala.monoids

import org.scalacheck.Gen
import org.scalatest.prop._
import org.scalatest.{Matchers, PropSpec}

abstract class MonoidLawsSpec[T](val monoid: Monoid[T], val gen: Gen[T]) extends PropSpec with PropertyChecks with Matchers {

  property("identity law") {
    forAll(gen) {
      (x: T) => monoid.op(x, monoid.zero) shouldBe x
    }
  }

  property("associativity law") {
    forAll(gen, gen, gen) {
      (x: T, y: T, z: T) => monoid.op(monoid.op(x, y), z) shouldBe monoid.op(x, monoid.op(y, z))
    }
  }
}

abstract class EndofuncitionMonoidLawsSpec[T](val monoid: Monoid[T => T],
                                              val valueGen: Gen[T],
                                              val endofunctionGen: Gen[T => T]) extends PropSpec with PropertyChecks with Matchers {

  property("identity law") {
    forAll(endofunctionGen) {
      (f: T => T) => forAll(valueGen) {
        (x: T) => monoid.op(f, monoid.zero).apply(x) shouldBe f.apply(x)
      }
    }
  }

  property("associativity law") {
    forAll(endofunctionGen, endofunctionGen, endofunctionGen) {
      (f: T => T, g: T => T, h: T => T) => forAll(valueGen) {
        (x: T) => monoid.op(monoid.op(f, g), h).apply(x) shouldBe monoid.op(f, monoid.op(g, h)).apply(x)
      }
    }

  }
}

object EndofuncitionMonoidLawsSpec {

  val linearFunctionGen = for {
    n <- Gen.choose(Int.MinValue, Int.MaxValue)
    m <- Gen.choose(Int.MinValue, Int.MaxValue)
  } yield (x: Int) => n * x + m

}

class IntAdditionSpec extends MonoidLawsSpec[Int](Monoid.intAddition, Gen.choose(Int.MinValue, Int.MaxValue))

class IntMultiplicationSpec extends MonoidLawsSpec[Int](Monoid.intMultiplication, Gen.choose(Int.MinValue, Int.MaxValue))

class BooleanOrSpec extends MonoidLawsSpec[Boolean](Monoid.booleanOr, Gen.oneOf(true, false))

class BolleanAndSpec extends MonoidLawsSpec[Boolean](Monoid.booleanAnd, Gen.oneOf(true, false))

class OptionIntMonoid extends MonoidLawsSpec[Option[Int]](Monoid.optionMonoid, Gen.option(Gen.choose(Int.MinValue, Int.MaxValue)))

class OptionBoolMonoid extends MonoidLawsSpec[Option[Boolean]](Monoid.optionMonoid, Gen.option(Gen.oneOf(true, false)))

class EndofunctionMonoid extends EndofuncitionMonoidLawsSpec[Int](
  Monoid.endoMonoid, Gen.choose(Int.MinValue, Int.MaxValue), EndofuncitionMonoidLawsSpec.linearFunctionGen)
