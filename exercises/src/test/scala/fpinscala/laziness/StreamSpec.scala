package fpinscala.laziness

import org.scalatest.{FlatSpec, Matchers}

class StreamSpec extends FlatSpec with Matchers {

  behavior of "Exercise 5.1"

  "Stream.toList" should "evaluate entire Stream and convert it to list" in {
    Stream(1, 2, 3, 4).toList shouldBe List(1, 2, 3, 4)
  }

  "Stream.toList" should "return an empty list for an empty stream" in {
    Stream().toList shouldBe List()
  }

  behavior of "Exercise 5.2"

  "Stream.take" should "return first n elements of a stream" in {
    Stream(1, 2, 3, 4, 5).take(2).toList shouldBe List(1, 2)
  }

  "Stream.take" should "return an empty stream, if we take 0 elements" in {
    Stream(1, 2, 3, 4, 5).take(0) shouldBe Empty
  }

  "Stream.take" should "return an entire stream, if we take all or more elements" in {
    Stream(1, 2, 3, 4, 5).take(5).toList shouldBe List(1, 2, 3, 4, 5)
    Stream(1, 2, 3, 4, 5).take(20).toList shouldBe List(1, 2, 3, 4, 5)
  }

  "Stream.drop" should "do nothing, if we drop 0 elements" in {
    Stream(1, 2, 3, 4).drop(0).toList shouldBe List(1, 2, 3, 4)
  }

  "Stream.drop" should "drop first n elements from a stream" in {
    Stream(1, 2, 3, 4).drop(2).toList shouldBe List(3, 4)
  }

  "Stream.drop" should "return an empty stream if we drop all or more elements" in {
    Stream(1, 2, 3, 4).drop(4) shouldBe Empty
    Stream(1, 2, 3, 4).drop(20) shouldBe Empty
  }

  behavior of "Exercise 5.3"

  "Stream.takeWhile" should "return all starting elements that match a predicate" in {
    Stream(1, 2, 3, 4).takeWhile(_ < 3).toList shouldBe List(1, 2)
  }

}
