package fpinscala.errorhandling

import org.scalatest.{FlatSpec, Matchers}

class OptionSpec extends FlatSpec with Matchers {

  behavior of "Exercise 4.1"

  "Option.map" should "do nothing for None" in {
    None.map(_.toString) shouldBe None
  }

  "Option.map" should "map value inside Some" in {
    Some(10).map(_ + 2) shouldBe Some(12)
  }

  "Option.getOrElse" should "return value inside Some" in {
    Some(10).getOrElse(12) shouldBe 10
  }

  "Option.getOrElse" should "return default value when None" in {
    None.getOrElse(12) shouldBe 12
  }

  "Option.flatMap" should "do nothing for None" in {
    None.flatMap(Some(_)) shouldBe None
  }

  "Option.flatMap" should "flatten tested Options" in {
    Some(10).flatMap(x => Some(x + 2)) shouldBe Some(12)
    Some(10).flatMap(_ => None) shouldBe None
  }

  "Option.orElse" should "return first option if defined" in {
    Some(1).orElse(None) shouldBe Some(1)
    Some(1).orElse(Some(2)) shouldBe Some(1)
  }

  "Option.orElse" should "return second option if the first is not defined" in {
    None.orElse(Some(1)) shouldBe Some(1)
    None.orElse(None) shouldBe None
  }

  "Option.filter" should "return None if filter predicate is not met" in {
    Some(1).filter(_ < 0) shouldBe None
  }

  "Option.filter" should "return Some if filter predicate is met" in {
    Some(1).filter(_ > 0) shouldBe Some(1)
  }

  "Option.filter" should "return None when None" in {
    None.filter(_ => true) shouldBe None
    None.filter(_ => false) shouldBe None
  }

  behavior of "Exercise 4.2"

  "Option.variance" should "return None for empty list" in {
    Option.variance(Seq()) shouldBe None
  }

  "Option.variance" should "return variance for non empty list" in {
    val testSeq = Seq(1.0, 1.0, -1.0, -1.0) // mean is 0 and all values 1 away from it
    Option.variance(testSeq) shouldBe Some(1.0)
  }

  behavior of "Exercise 4.3"

  "Option.map2" should "return None if any of input values is None" in {
    val op = (_: Int, _: Int) => 1
    Option.map2(None, None)(op) shouldBe None
    Option.map2(None, Some(1))(op) shouldBe None
    Option.map2(Some(1), None)(op) shouldBe None
  }

  "Option.map2" should "combine two values wrapped in Some object" in {
    Option.map2(Some(2), Some(2.0))(_ * _) shouldBe Some(4.0)
  }

  "Option.map2_2" should "return None if any of input values is None" in {
    val op = (_: Int, _: Int) => 1
    Option.map2_2(None, None)(op) shouldBe None
    Option.map2_2(None, Some(1))(op) shouldBe None
    Option.map2_2(Some(1), None)(op) shouldBe None
  }

  "Option.map2_2" should "combine two values wrapped in Some object" in {
    Option.map2_2(Some(2), Some(2.0))(_ * _) shouldBe Some(4.0)
  }

  behavior of "Exercise 4.4"

  "Option.sequence" should "return None if a single object in a sequence is None" in {
    Option.sequence(List(None)) shouldBe None
    Option.sequence(List(None, Some(1))) shouldBe None
    Option.sequence(List(Some(1), None)) shouldBe None
    Option.sequence(List(Some(1), None, Some(2))) shouldBe None
  }

  "Option.sequence" should "return wrapped List of all list elements when no None in the list" in {
    Option.sequence(List(Some(1), Some(2), Some(3))) shouldBe Some(List(1, 2, 3))
  }

  behavior of "Exercise 4.5"

  "Option.traverse" should "return empty list for an empty list" in {
    Option.traverse(List())(_ => None) shouldBe Some(List())
  }

  "Option.traverse" should "return None if one of objects map to None" in {
    Option.traverse(List(1, 2, 3, 4))(a => if (a == 2) None else Some(a)) shouldBe None
  }

  "Option.traverse" should "return Option with a list of mapped values, when nothing mapped to None" in {
    Option.traverse(List(1, 2, 3, 4))(a => Some(a * 2)) shouldBe Some(List(2, 4, 6, 8))
  }


}
