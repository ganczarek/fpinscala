package fpinscala.monads

import org.scalacheck.Gen
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}

import scala.language.higherKinds

class MonadLawsSpec[A, B, C, D, M[_]](val monad: Monad[M],
                                      val valueGen: Gen[A],
                                      val genA: Gen[A => M[B]],
                                      val genB: Gen[B => M[C]],
                                      val genC: Gen[C => M[D]])
  extends PropSpec with PropertyChecks with Matchers {

  import monad._

  property("associativity law expressed with compose") {
    forAll(genA, genB, genC) {
      (x: A => M[B], y: B => M[C], z: C => M[D]) => forAll(valueGen) {
        (value: A) => compose(compose(x, y), z)(value) shouldBe compose(x, compose(y, z))(value)
      }
    }
  }

  property("left identity law expressed with compose") {
    forAll(genA) {
      (f: A => M[B]) => forAll(valueGen) {
        (a: A) => compose(f, (x: B) => unit(x))(a) shouldBe f(a)
      }
    }
  }

  property("right identity law expressed with compose") {
    forAll(genA) {
      (f: A => M[B]) => forAll(valueGen) {
        (a: A) => compose((x: A) => unit(x), f)(a) shouldBe f(a)
      }
    }
  }
}

class MonadLawsSpecWithFlatMap[A, B, C, D, M[_]](val monad: Monad[M],
                                                 val valueGen: Gen[A],
                                                 val genA: Gen[A => M[B]],
                                                 val genB: Gen[B => M[C]],
                                                 val genC: Gen[C => M[D]])
  extends PropSpec with PropertyChecks with Matchers {

  import monad._

  property("associativity law expressed with flatMap") {
    forAll(genA, genB, genC) {
      (x: A => M[B], y: B => M[C], z: C => M[D]) => forAll(valueGen) {
        (value: A) => flatMap(x(value))(b => flatMap(y(b))(z)) shouldBe flatMap(flatMap(x(value))(y))(z)
      }
    }
  }

  property("left identity law expressed with flatMap") {
    forAll(genA) {
      (f: A => M[B]) => forAll(valueGen) {
        (a: A) => flatMap(f(a))(unit(_)) shouldBe f(a)
      }
    }
  }

  property("right identity law expressed with flatMap") {
    forAll(genA) {
      (f: A => M[B]) => forAll(valueGen) {
        (a: A) => flatMap(unit(a))(f) shouldBe f(a)
      }
    }
  }
}

// flatMap is a map with join
class MonadLawsSpecWithJoin[A, B, C, D, M[_]](val monad: Monad[M],
                                              val valueGen: Gen[A],
                                              val genA: Gen[A => M[B]],
                                              val genB: Gen[B => M[C]],
                                              val genC: Gen[C => M[D]])
  extends PropSpec with PropertyChecks with Matchers {

  import monad._

  property("associativity law expressed with flatMap") {
    forAll(genA, genB, genC) {
      (x: A => M[B], y: B => M[C], z: C => M[D]) => forAll(valueGen) {
        (value: A) => join(map(x(value))(b => join(map(y(b))(z)))) shouldBe join(map(join(map(x(value))(y)))(z))
      }
    }
  }

  property("left identity law expressed with flatMap") {
    forAll(genA) {
      (f: A => M[B]) => forAll(valueGen) {
        (a: A) => join(map(f(a))(unit(_))) shouldBe f(a)
      }
    }
  }

  property("right identity law expressed with flatMap") {
    forAll(genA) {
      (f: A => M[B]) => forAll(valueGen) {
        (a: A) => join(map(unit(a))(f)) shouldBe  f(a)
      }
    }
  }
}

object MonadLawsSpec {

  def shortToIntLinearFunctionGen[M[_]](monad: Monad[M]): Gen[Short => M[Int]] = for {
    n <- Gen.choose(Short.MinValue, Short.MaxValue)
    m <- Gen.choose(Short.MinValue, Short.MaxValue)
  } yield (x: Short) => monad.unit(n * x + m)

  def intToLongLinearFunctionGen[M[_]](monad: Monad[M]): Gen[Int => M[Long]] = for {
    n <- Gen.choose(Int.MinValue, Int.MaxValue)
    m <- Gen.choose(Int.MinValue, Int.MaxValue)
  } yield (x: Int) => monad.unit((n * x + m).toLong)

  def longToStringLinearFunctionGen[M[_]](monad: Monad[M]): Gen[Long => M[String]] = for {
    n <- Gen.choose(Long.MinValue, Long.MaxValue)
    m <- Gen.choose(Long.MinValue, Long.MaxValue)
  } yield (x: Long) => monad.unit((n * x + m).toString)

}

class OptionMonadLawSpec extends MonadLawsSpec[Short, Int, Long, String, Option](Monad.optionMonad,
  Gen.choose(Short.MinValue, Short.MaxValue),
  MonadLawsSpec.shortToIntLinearFunctionGen(Monad.optionMonad),
  MonadLawsSpec.intToLongLinearFunctionGen(Monad.optionMonad),
  MonadLawsSpec.longToStringLinearFunctionGen(Monad.optionMonad)
)

class OptionMonadLawSpecWithFlatMap extends MonadLawsSpecWithFlatMap[Short, Int, Long, String, Option](Monad.optionMonad,
  Gen.choose(Short.MinValue, Short.MaxValue),
  MonadLawsSpec.shortToIntLinearFunctionGen(Monad.optionMonad),
  MonadLawsSpec.intToLongLinearFunctionGen(Monad.optionMonad),
  MonadLawsSpec.longToStringLinearFunctionGen(Monad.optionMonad)
)

class OptionMonadLawSpecWithJoin extends MonadLawsSpecWithJoin[Short, Int, Long, String, Option](Monad.optionMonad,
  Gen.choose(Short.MinValue, Short.MaxValue),
  MonadLawsSpec.shortToIntLinearFunctionGen(Monad.optionMonad),
  MonadLawsSpec.intToLongLinearFunctionGen(Monad.optionMonad),
  MonadLawsSpec.longToStringLinearFunctionGen(Monad.optionMonad)
)

class ListMonadLawSpec extends MonadLawsSpec[Short, Int, Long, String, List](Monad.listMonad,
  Gen.choose(Short.MinValue, Short.MaxValue),
  MonadLawsSpec.shortToIntLinearFunctionGen(Monad.listMonad),
  MonadLawsSpec.intToLongLinearFunctionGen(Monad.listMonad),
  MonadLawsSpec.longToStringLinearFunctionGen(Monad.listMonad)
)

class ListMonadLawSpecWithFlatMap extends MonadLawsSpecWithFlatMap[Short, Int, Long, String, List](Monad.listMonad,
  Gen.choose(Short.MinValue, Short.MaxValue),
  MonadLawsSpec.shortToIntLinearFunctionGen(Monad.listMonad),
  MonadLawsSpec.intToLongLinearFunctionGen(Monad.listMonad),
  MonadLawsSpec.longToStringLinearFunctionGen(Monad.listMonad)
)

class ListMonadLawSpecWithJoin extends MonadLawsSpecWithJoin[Short, Int, Long, String, List](Monad.listMonad,
  Gen.choose(Short.MinValue, Short.MaxValue),
  MonadLawsSpec.shortToIntLinearFunctionGen(Monad.listMonad),
  MonadLawsSpec.intToLongLinearFunctionGen(Monad.listMonad),
  MonadLawsSpec.longToStringLinearFunctionGen(Monad.listMonad)
)
