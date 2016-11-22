package fpinscala.monoids

import org.scalacheck.{Gen, Properties}
import org.scalacheck.Prop.forAll

class MonoidLawsSpec[T](override val name: String, val monoid: Monoid[T], val gen: Gen[T]) extends Properties(monoid.getClass.getName) {

  property("identity law") = forAll(gen) {
    (x: T) => monoid.op(x, monoid.zero) == x
  }

  property("associativity law") = forAll(gen, gen, gen) {
    (x: T, y: T, z: T) => monoid.op(monoid.op(x, y), z) == monoid.op(x, monoid.op(y, z))
  }

}

class EndofuncitionMonoidLawsSpec[T](override val name: String,
                                     val monoid: Monoid[T => T],
                                     val valueGen: Gen[T],
                                     val endofunctionGen: Gen[T => T]) extends Properties(monoid.getClass.getName) {

  property("identity law") = forAll(endofunctionGen) {
    (f: T => T) => forAll(valueGen) {
      (x:T) => monoid.op(f, monoid.zero).apply(x) == f.apply(x)
    }
  }

  property("associativity law") = forAll(endofunctionGen, endofunctionGen, endofunctionGen) {
    (f: T => T, g: T => T, h: T => T) => forAll(valueGen) {
      (x:T) => monoid.op(monoid.op(f, g), h).apply(x) == monoid.op(f, monoid.op(g, h)).apply(x)
    }
  }

}

object EndofuncitionMonoidLawsSpec {

  val linearFunctionGen = for {
    n <- Gen.choose(Int.MinValue, Int.MaxValue)
    m <- Gen.choose(Int.MinValue, Int.MaxValue)
  } yield (x: Int) => n * x + m

}

object IntAdditionSpec extends MonoidLawsSpec[Int]("intAddition", Monoid.intAddition, Gen.choose(Int.MinValue, Int.MaxValue))

object IntMultiplicationSpec extends MonoidLawsSpec[Int]("intMultiplication", Monoid.intMultiplication, Gen.choose(Int.MinValue, Int.MaxValue))

object BooleanOrSpec extends MonoidLawsSpec[Boolean]("booleanOr", Monoid.booleanOr, Gen.oneOf(true, false))

object BolleanAndSpec extends MonoidLawsSpec[Boolean]("booleanAnd", Monoid.booleanAnd, Gen.oneOf(true, false))

object OptionIntMonoid extends MonoidLawsSpec[Option[Int]]("optionIntMonoid", Monoid.optionMonoid, Gen.option(Gen.choose(Int.MinValue, Int.MaxValue)))

object OptionBoolMonoid extends MonoidLawsSpec[Option[Boolean]]("optionBoolMonoid", Monoid.optionMonoid, Gen.option(Gen.oneOf(true, false)))

object EndofunctionMonoid extends EndofuncitionMonoidLawsSpec[Int]("endoIntMonoid", Monoid.endoMonoid,
  Gen.choose(Int.MinValue, Int.MaxValue), EndofuncitionMonoidLawsSpec.linearFunctionGen)
