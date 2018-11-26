package fpinscala.applicative

import fpinscala.applicative.Applicative.{listApplicative, optionApplicative}
import fpinscala.testing.Prop.Passed
import fpinscala.testing.{Gen, Prop}
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

  behavior of "Exercise 12.12"

  "Applicative.sequenceMap" should "perform sequence over Map" in {
    optionApplicative.sequenceMap(Map("1" -> Some(1), "2" -> Some(2))) shouldBe Some(Map("1" -> 1, "2" -> 2))
    optionApplicative.sequenceMap(Map("1" -> Some(1), "2" -> None)) shouldBe None
  }

  behavior of "Exercise 12.13 and 12.14"

  "Traverse.listTraverse.sequence" should "return None if any of the inputs of a list is None" in {
    Traverse.listTraverse.sequence(List(Some(1), None, Some(2)))(optionApplicative) shouldBe None
    Traverse.listTraverse.sequence(List(None))(optionApplicative) shouldBe None
  }

  "Traverse.listTraverse.sequence" should "return list with all values, if there are no None" in {
    Traverse.listTraverse.sequence[Option, Int](List(Some(1), Some(2), Some(3)))(optionApplicative) shouldBe Some(List(1, 2, 3))
    Traverse.listTraverse.sequence[Option, Int](List(Some(1)))(optionApplicative) shouldBe Some(List(1))
  }

  "Traverse.optionTraverse.sequence" should "return list with None if option is None" in {
    Traverse.optionTraverse.sequence(None)(listApplicative) shouldBe List(None)
    Traverse.optionTraverse.sequence(None)(listApplicative) shouldBe List(None)
  }

  "Traverse.optionTraverse.sequence" should "return first element from a list wrapped in Some, when not a None" in {
    Traverse.optionTraverse.sequence(Some(List(1, 2, 3)))(listApplicative) shouldBe List(Some(1))
  }

  "Traverse.treeTraverse.sequence" should "return None, if tree contains a None" in {
    Traverse.treeTraverse.sequence(Tree(Some(1), List(Tree(Some(1), Nil), Tree(None, Nil))))(optionApplicative) shouldBe None
    Traverse.treeTraverse.sequence(Tree(None, Nil))(optionApplicative) shouldBe None
  }

  "Traverse.treeTraverse.sequence" should "return entire tree wrapped in Option, if tree does not contain a None" in {
    val treeOfOptions = Tree(Some(1), List(Tree(Some(2), Nil), Tree(Some(3), Nil)))
    val treeWrappedInOption = Some(Tree(1, List(Tree(2, Nil), Tree(3, Nil))))

    Traverse.treeTraverse.sequence[Option, Int](treeOfOptions)(optionApplicative) shouldBe treeWrappedInOption
  }

  behavior of "Exercise 12.16"

  "Traverse.reverse" should "reverse any functor" in {
    import Traverse.listTraverse.{reverse, toList}

    val listGenerator = Gen.listOf(Gen.choose(-100, 100))
    val gen = listGenerator ** listGenerator

    val prop = Prop.forAll(gen) {
      case (x, y) => toList(reverse(x)) ++ toList(reverse(y)) == reverse(toList(y) ++ toList(x))
    }

    Prop.run(prop) shouldBe Passed
  }

  behavior of "Exercise 12.17"

  "Traverse.foldLeft" should "fold feft with use of mapAccum" in {
    val testList = List(1, 2, 3, 4, 5, 6, 7, 8)
    Traverse.listTraverse.foldLeft(testList)(0)(_ * _) shouldBe testList.foldLeft(0)(_ * _)
  }

  behavior of "Exercise 12.18"

  "Traverse.fuse" should "traverse once and apply both fused functors" in {
    implicit val a: Applicative[Option] = optionApplicative
    implicit val b: Applicative[List] = listApplicative

    val result = Traverse.listTraverse.fuse[Option, List, Int, Int](List(1, 2, 3))(Some(_), List(_))

    result shouldBe(Some(List(1, 2, 3)), List(List(1, 2, 3)))
  }

  behavior of "Exercise 12.19"

  "Traverse.compose" should "traverse nested Traversals" in {
    implicit val a: Applicative[Option] = optionApplicative
    implicit val b: Applicative[List] = listApplicative

    val composedTraverse = Traverse.listTraverse.compose(Traverse.optionTraverse)
    val maybeInts = List(Some(1), None, Some(2))
    val expected = Some(List(Some(3), None, Some(4)))

    composedTraverse.traverse[Option, Int, Int](maybeInts)(a => Option(a + 2)) shouldBe expected
  }

}
