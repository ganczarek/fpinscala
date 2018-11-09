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

}
