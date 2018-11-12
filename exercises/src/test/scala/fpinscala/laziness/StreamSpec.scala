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

  behavior of "Exercise 5.4"

  "Stream.forAll" should "return true for empty stream" in {
    Stream().forAll(_ => false) shouldBe true
  }

  "Stream.forAll" should "should evaluate all elements, when predicate matches all of them" in {
    var x = 0
    Stream( () => {x=x+1; 1}, () => {x=x+1; 2}, () => {x=x+1; 3}).forAll(z => z() > 0) shouldBe true
    x shouldBe 3
  }

  "Stream.forAll" should "should evaluate elements till first predicate failure" in {
    var x = 0
    Stream( () => {x=x+1; 1}, () => {x=x+1; -2}, () => {x=x+1; 3}).forAll(z => z() > 0) shouldBe false
    x shouldBe 2
  }

  behavior of "Exercise 5.5"

  "Stream.takeWhileWithFoldRight" should "return all starting elements that match a predicate" in {
    Stream(1, 2, 3, 4).takeWhileWithFoldRight(_ < 3).toList shouldBe List(1, 2)
  }

  behavior of "Exercise 5.6"

  "Stream.headOption" should "return None if stream is empty" in {
    Stream().headOption shouldBe None
  }

  "Stream.headOption" should "return head of the stream" in {
    Stream(1, 2, 3).headOption shouldBe Some(1)
  }

  behavior of "Exercise 5.7"

  "Stream.map" should "return empty stream, when empty" in {
    Stream().map(_.toString) shouldBe Stream()
  }

  "Stream.map" should "map all stream elements" in {
    Stream(1, 2, 3).map(_.toString).toList shouldBe List("1", "2", "3")
  }

  "Stream.filter" should "return empty stream, when empty" in {
    Stream().filter(_ => true) shouldBe Stream()
  }

  "Stream.filter" should "filter out all elements not matching a predicate" in {
    Stream(4, 1, 3, 2).filter(_ > 2).toList shouldBe List(4, 3)
  }

  "Stream.append" should "append a stream to an empty stream in non-strict way" in {
    Stream().append(Stream(12)).toList shouldBe List(12)
  }

  "Stream.append" should "append one stream to another" in {
    Stream(1, 2, 3).append(Stream(4, 5, 6)).toList shouldBe List(1, 2, 3, 4, 5, 6)
  }

  "Stream.append" should "append Stream[A] and Stream[B], when B is supertype of A" in {
    val testStream = Stream[List[Int]](List(1, 2), List(3))
    val streamToAppend = Stream[Seq[Int]](Seq(4))
    val expectedStream = Stream(Seq(1, 2), Seq(3), Seq(4))
    testStream.append(streamToAppend).toList shouldBe expectedStream.toList
  }

  "Stream.flatMap" should "return empty stream, when empty" in {
    Stream().flatMap(a => Stream(a.toString)) shouldBe Stream()
  }

  "Stream.flatMap" should "map and flatten stream elements" in {
    Stream(1, 2, 3).flatMap(a => Stream(a, a)).toList shouldBe List(1, 1, 2, 2, 3, 3)
  }

  behavior of "Exercise 5.8"

  "Stream.constant" should "return infinite stream of the same values" in {
    Stream.constant(5).take(5).toList shouldBe List(5, 5, 5, 5, 5)
    Stream.constant(2).take(7).toList shouldBe List(2, 2, 2, 2, 2, 2, 2)
  }

  behavior of "Exercise 5.9"

  "Stream.from" should "return infinite stream of increasing integers" in {
    Stream.from(10).take(5).toList shouldBe List(10, 11, 12, 13, 14)
    Stream.from(2).take(3).toList shouldBe List(2, 3, 4)
  }

  behavior of "Exercise 5.10"

  "Stream.fibs" should "return infinite stream of Fibonacci numbers" in {
    Stream.fibs().take(8).toList shouldBe List(0, 1, 1, 2, 3, 5, 8, 13)
  }

}
