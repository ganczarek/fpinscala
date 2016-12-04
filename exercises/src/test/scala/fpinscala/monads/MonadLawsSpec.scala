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

  property("associativity law") {
    forAll(genA, genB, genC) {
      (x: A => M[B], y: B => M[C], z: C => M[D]) => forAll(valueGen) {
        (value: A) => monad.compose(monad.compose(x, y), z).apply(value) shouldBe monad.compose(x, monad.compose(y, z)).apply(value)
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

class ListMonadLawSpec extends MonadLawsSpec[Short, Int, Long, String, List](Monad.listMonad,
  Gen.choose(Short.MinValue, Short.MaxValue),
  MonadLawsSpec.shortToIntLinearFunctionGen(Monad.listMonad),
  MonadLawsSpec.intToLongLinearFunctionGen(Monad.listMonad),
  MonadLawsSpec.longToStringLinearFunctionGen(Monad.listMonad)
)
