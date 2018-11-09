package fpinscala.errorhandling

import org.scalatest.{FlatSpec, Matchers}

class EitherSpec extends FlatSpec with Matchers {

  behavior of "Exercise 4.6"

  "Either.map" should "do nothing when Left value" in {
    Left("string").map(_ => 5) shouldBe Left("string")
  }

  "Either.map" should "do the map function when Right value" in {
    Right(10) map (_ + 2) shouldBe Right(12)
  }

  "Either.flatMap" should "do nothing when Left value" in {
    Left("string") flatMap (a => Right(a)) shouldBe Left("string")
  }

  "Either.flatMap" should "return Left when mapping returns Left" in {
    Right(10) flatMap (_ => Left("test")) shouldBe Left("test")
  }

  "Either.flatMap" should "return Right when mapping is successful" in {
    Right(10) flatMap (a => Right(a * 2)) shouldBe Right(20)
  }

  "Either.orElse" should "do nothing when Right" in {
    Right(10).orElse(Right("test")) shouldBe Right(10)
  }

  "Either.orElse" should "return other value when Left" in {
    Left("string").orElse(Right(19)) shouldBe Right(19)
    Left("string").orElse(Left("test")) shouldBe Left("test")
  }

  "Either.map2" should "return do nothing if any side of map function is Left" in {
    val op = (_: Int, _: Int) => 1
    Left("string").map2(Left(1))(op) shouldBe Left("string")
    Left("string").map2(Right(1))(op) shouldBe Left("string")
    Right(1).map2(Left("string"))(op) shouldBe Left("string")
  }

  "Either.map2" should "map values if both wrapped in Right object" in {
    Right(2).map2(Right(5.0))(_ * _) shouldBe Right(10.0)
  }

  behavior of "Exercise 4.7"

  "Either.sequence" should "return first Left object in a sequence" in {
    Either.sequence(List(Left(1), Left(2), Left(3))) shouldBe Left(1)
    Either.sequence(List(Right(1), Left(2), Left(3))) shouldBe Left(2)
    Either.sequence(List(Right(1), Right(2), Left(3))) shouldBe Left(3)
  }

  "Either.sequence" should "return wrapper list of Right values when no Left values" in {
    Either.sequence(List(Right(1), Right(2), Right(3))) shouldBe Right(List(1, 2, 3))
  }

  "Either.traverse" should "return first Left returned by map function" in {
    Either.traverse(List(1, 2, 3))(a => Left(a)) shouldBe Left(1)
  }

  "Either.traverse" should "return list of mapped values wrapped in Right object, if no Left objects" in {
    Either.traverse(List(1, 2, 3))(a => Right(a * 2)) shouldBe Right(List(2, 4, 6))
  }

}
