package fpinscala.applicative

import org.scalatest.{FlatSpec, Matchers}

class ApplicativeSpec extends FlatSpec with Matchers {

  behavior of "Exercise 12.1"

  val listApplicative = new Applicative[List] {
    override def unit[A](a: => A): List[A] = List(a)

    override def map2[A, B, C](fa: List[A], fb: List[B])(f: (A, B) => C): List[C] = {
      for {
        (a, b) <- fa zip fb
      } yield f(a, b)
    }
  }

  val optionApplicative = new Applicative[Option] {
    override def unit[A](a: => A): Option[A] = Option(a)

    override def map2[A, B, C](fa: Option[A], fb: Option[B])(f: (A, B) => C): Option[C] = for {
      a <- fa
      b <- fb
    } yield f(a, b)
  }

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

}
