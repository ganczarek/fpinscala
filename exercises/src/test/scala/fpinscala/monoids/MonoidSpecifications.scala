package fpinscala.monoids

import org.scalacheck.{Gen, Properties}
import org.scalacheck.Prop.forAll

class MonoidLawsSpec[T](override val name: String, val monoid: Monoid[T], val gen: Gen[T]) extends Properties(monoid.getClass.getName) {

  property("identity law") = forAll (gen) {
    (x: T) => monoid.op(x, monoid.zero) == x
  }

  property("associativity law") = forAll (gen, gen, gen) {
    (x: T, y: T, z: T) => monoid.op(monoid.op(x, y), z) == monoid.op(x, monoid.op(y, z))
  }
}

object IntAdditionSpec extends MonoidLawsSpec[Int]("intAddition", Monoid.intAddition, Gen.choose(Int.MinValue, Int.MaxValue))
object IntMultiplicationSpec extends MonoidLawsSpec[Int]("intMultiplication", Monoid.intMultiplication, Gen.choose(Int.MinValue, Int.MaxValue))
object BooleanOrSpec extends MonoidLawsSpec[Boolean]("booleanOr", Monoid.booleanOr, Gen.oneOf(true, false))
object BolleanAndSpec extends MonoidLawsSpec[Boolean]("booleanAnd", Monoid.booleanAnd, Gen.oneOf(true, false))
object OptionIntMonoid extends MonoidLawsSpec[Option[Int]]("optionIntMonoid", Monoid.optionMonoid, Gen.option(Gen.choose(Int.MinValue, Int.MaxValue)))
object OptionBoolMonoid extends MonoidLawsSpec[Option[Boolean]]("optionBoolMonoid", Monoid.optionMonoid, Gen.option(Gen.oneOf(true, false)))
