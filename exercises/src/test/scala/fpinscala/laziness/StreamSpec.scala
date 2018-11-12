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

  behavior of "Exercise 5.11"

  "Stream.unfold" should "unfold an infinite stream" in {
    val startState: Int = 4
    val nextState = (s: Int) => s + 2
    val stateValue = (s: Int) => s * 2
    val testStream = Stream.unfold(startState)(state => Some((stateValue(state), nextState(state))))

    testStream.take(4).toList shouldBe List(8, 12, 16, 20)
  }

  "Stream.unfold" should "unfold a finite stream" in {
    Stream.unfold(0)(s => if (s < 4) Some((s, s + 1)) else None).toList shouldBe List(0, 1, 2, 3)
  }

  behavior of "Exercise 5.12"

  "Stream.fibs_1" should "return infinite stream of Fibonacci numbers" in {
    Stream.fibs_1().take(8).toList shouldBe List(0, 1, 1, 2, 3, 5, 8, 13)
  }

  "Stream.from_1" should "return infinite stream of increasing integers" in {
    Stream.from_1(10).take(5).toList shouldBe List(10, 11, 12, 13, 14)
    Stream.from_1(2).take(3).toList shouldBe List(2, 3, 4)
  }

  "Stream.constant_1" should "return infinite stream of the same values" in {
    Stream.constant_1(5).take(5).toList shouldBe List(5, 5, 5, 5, 5)
    Stream.constant_1(2).take(7).toList shouldBe List(2, 2, 2, 2, 2, 2, 2)
  }

  "Stream.ones_1" should "return infinite stream of 1s" in {
    Stream.ones_1().take(5).toList shouldBe List(1, 1, 1, 1, 1)
    Stream.ones_1().take(3).toList shouldBe List(1, 1, 1)
  }

  behavior of "Exercise 5.13"

  "Stream.mapWithUnfold" should "map values in a stream" in {
    Stream(1, 2, 3).mapWithUnfold(_.toString).toList shouldBe List("1", "2", "3")
  }

  "Stream.takeWithUnfold" should "return empty stream when taking zero elements" in {
    Stream(1, 2, 3).takeWithUnfold(0) shouldBe Empty
  }

  "Stream.takeWithUnfold" should "return first n elements of a stream" in {
    Stream(1, 2, 3, 4, 5).takeWithUnfold(2).toList shouldBe List(1, 2)
  }

  "Stream.takeWithUnfold" should "return an empty stream, if we take 0 elements" in {
    Stream(1, 2, 3, 4, 5).takeWithUnfold(0) shouldBe Empty
  }

  "Stream.takeWithUnfold" should "return an entire stream, if we take all or more elements" in {
    Stream(1, 2, 3, 4, 5).takeWithUnfold(5).toList shouldBe List(1, 2, 3, 4, 5)
    Stream(1, 2, 3, 4, 5).takeWithUnfold(20).toList shouldBe List(1, 2, 3, 4, 5)
  }

  "Stream.takeWhileWithUnfold" should "return all starting elements that match a predicate" in {
    Stream(1, 2, 3, 4).takeWhileWithUnfold(_ < 3).toList shouldBe List(1, 2)
  }

  "Stream.zipWith" should "zip two stream" in {
    Stream(1, 2, 3).zipWith(Stream(1, 2)).toList shouldBe List((1, 1), (2, 2))
    Stream(1, 2).zipWith(Stream(1, 2, 3)).toList shouldBe List((1, 1), (2, 2))
    Stream(1, 2, 3).zipWith(Stream(1, 2, 3)).toList shouldBe List((1, 1), (2, 2), (3, 3))
  }

  "Stream.zipAllWithUnfold" should "zip all elements in two streams, when stream is shorter" in {
    Stream(1, 2).zipAllWithUnfold(Stream(5, 6, 7)).toList shouldBe List(
      (Some(1), Some(5)), (Some(2), Some(6)), (None, Some(7))
    )
  }

  "Stream.zipAllWithUnfold" should "zip all elements in two streams, when stream is longer" in {
    Stream(1, 2, 3).zipAllWithUnfold(Stream(5, 6)).toList shouldBe List(
      (Some(1), Some(5)), (Some(2), Some(6)), (Some(3), None)
    )
  }

  "Stream.zipAllWithUnfold()" should "zip all elements in two streams, when stream are of equal length" in {
    Stream(1, 2, 3).zipAllWithUnfold(Stream(5, 6, 7)).toList shouldBe List(
      (Some(1), Some(5)), (Some(2), Some(6)), (Some(3), Some(7))
    )
  }

  behavior of "Exercise 5.14"

  "Stream.startsWith" should "work fine with empty streams" in {
    Stream(1, 2, 3) startsWith Stream() shouldBe true
    Stream() startsWith Stream() shouldBe true
    Stream() startsWith Stream(1, 2) shouldBe false
  }

  "Stream.startsWith" should "return false when stream doesn't start with given stream" in {
    Stream(1, 2, 3) startsWith Stream(2, 3, 1) shouldBe false
    Stream(1, 2) startsWith Stream(1, 2, 3) shouldBe false
  }

  "Stream.startsWith" should "return true when stream does start with given stream" in {
    Stream(1, 2, 3) startsWith Stream(1) shouldBe true
    Stream(1, 2, 3) startsWith Stream(1, 2) shouldBe true
    Stream(1, 2, 3) startsWith Stream(1, 2, 3) shouldBe true
  }

  behavior of "Exercise 5.15"

  "Stream.tails" should "return an empty stream as a tail of an empty stream" in {
    Stream().tails.toList.map(_.toList) shouldBe List(List())
  }

  "Stream.tails" should "return a stream of suffixes of the input sequences" in {
    Stream(1, 2, 3).tails.toList.map(_.toList) shouldBe List(List(1, 2, 3), List(2, 3), List(3), List())
  }
}
