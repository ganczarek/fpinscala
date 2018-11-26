package fpinscala.applicative

import fpinscala.applicative.Applicative.{listApplicative, optionApplicative}
import org.scalatest.{FlatSpec, Matchers}

class ApplicativeSpec extends FlatSpec with Matchers {

  behavior of "Exercise 12.1"

  "Applicative.sequence" should "turn list of functors into a functor of a list" in {
    listApplicative.sequence(List(List(1), List(2), List(3))) shouldBe List(List(1, 2, 3))
  }

  "Applicative.travers" should "turn list of functors into a functor of a list" in {
    listApplicative.traverse(List(1, 2, 3))(x => List(x)) shouldBe List(List(1, 2, 3))
  }

  "Applicative.replicateM" should "replicate a functor given number of times" in {
    listApplicative.replicateM(5, List(1)) shouldBe List(List(1, 1, 1, 1, 1))
  }

  "Applicative.apply" should "apply for list applicative functor" in {
    listApplicative.apply(List[Int => Int](_ + 1, _ * 10))(List(1, 2)) shouldBe List(2, 20)
  }

  "Applicative.apply" should "apply for option applicative functor" in {
    optionApplicative.apply(Some[Int => Int](_ + 1))(Some(1)) shouldBe Some(2)
    optionApplicative.apply(None)(Some(1)) shouldBe None
    optionApplicative.apply(Some[Int => Int](_ + 1))(None) shouldBe None
  }

  "Applicative.product" should "return product of two applicative functors" in {
    listApplicative.product(optionApplicative).unit(1) shouldBe(List(1), Some(1))
  }

  "Applicative.factor" should "zip two lists" in {
    listApplicative.factor(List(1, 2, 3), List("a", "b", "c")) shouldBe List((1, "a"), (2, "b"), (3, "c"))
  }

  "Applicative.factor" should "combine two optional values" in {
    optionApplicative.factor(Some(1), Some(2)) shouldBe Some((1, 2))
    optionApplicative.factor(Some(1), None) shouldBe None
    optionApplicative.factor(None, None) shouldBe None
  }

  behavior of "Exercise 12.2"

  val applyAndUnitOptionApplicative = new Applicative[Option] {
    override def unit[A](a: => A): Option[A] = Option(a)

    override def apply[A, B](fab: Option[A => B])(fa: Option[A]): Option[B] = for {
      aToB <- fab
      a <- fa
    } yield aToB(a)
  }

  "Applicative.map" should "be implemented in terms of apply and unit" in {
    applyAndUnitOptionApplicative.map(Some(1))(_ + 2) shouldBe Some(3)
    applyAndUnitOptionApplicative.map(None)(identity) shouldBe None
  }

  "Applicative.map2" should "be implemented in terms of apply and unit" in {
    applyAndUnitOptionApplicative.map2(Some(1), Some(2))(_ + _) shouldBe Some(3)
    applyAndUnitOptionApplicative.map2(Some(1), None)(_ + _) shouldBe None
    applyAndUnitOptionApplicative.map2(None, Some(1))((_, _) => ()) shouldBe None
    applyAndUnitOptionApplicative.map2(None, None)((_, _) => ()) shouldBe None
  }

  "Applicative.apply" should "be implemented in terms of map2 and unit" in {
    listApplicative.apply(List[Int => Int](_ + 1, _ * 10))(List(1, 2)) shouldBe List(2, 20)
  }

  behavior of "Exercise 12.3"

  "Applicative.map3" should "map 3 functors" in {
    optionApplicative.map3(Some(1), Some(2), Some(3))(_ + _ - _) shouldBe Some(0)
  }

  "Applicative.map4" should "map 4 functors" in {
    optionApplicative.map4(Some(1), Some(2), Some(3), Some(4))(_ + _ - _ + _) shouldBe Some(4)
  }

  behavior of "Exercise 12.4"

  "StreamApplicative.sequence" should "zip streams" in {
    val sreams = List(Stream(1, 2), Stream("a", "b"), Stream(3, 4))
    Applicative.streamApplicative.sequence(sreams) shouldBe Stream(List(1, "a", 3), List(2, "b", 4))
  }

  behavior of "Exercise 12.5"

  "EitherMonad.map" should "map right value when present" in {
    Monad.eitherMonad.map(Right(10))(_ + 5) shouldBe Right(15)
  }

  "EitherMonad.map" should "return pass through error message" in {
    Monad.eitherMonad.map(Left[String, Int]("Error Message"))(_ + 5) shouldBe Left("Error Message")
  }

  behavior of "Exercise 12.6"

  "Validation" should "return value if all validation pass" in {
    Applicative.validationApplicative.map3(
      Success(1),
      Success("2"),
      Success(3.0)
    )((_, _, _)) shouldBe Success((1, "2", 3.0))
  }

  "Validation" should "accumulate errors" in {
    Applicative.validationApplicative.map3(
      Failure("head1", Vector("tail1")),
      Success("2"),
      Failure("head2", Vector("tail2"))
    )((_, _, _)) shouldBe Failure("head2", Vector("tail2", "head1", "tail1"))
  }

  behavior of "Exercise 12.8"

  "Applicative.product" should "create a product of two applicatives" in {
    optionApplicative.product(listApplicative).map(Some(1), List(12))(_ * 2) shouldBe(Some(2), List(24))
    optionApplicative.product(listApplicative).map(None, List(12))(_ * 2) shouldBe(None, List(24))
  }

  behavior of "Exercise 12.9"

  "Applicative.compose" should "compose two applicatives" in {
    optionApplicative.compose(listApplicative).sequence(List(Some(List(1)), Some(List(2)))) shouldBe Some(List(List(1, 2)))
    optionApplicative.compose(listApplicative).sequence(List(Some(List(1)), None)) shouldBe None
  }
}
