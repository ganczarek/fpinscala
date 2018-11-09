package fpinscala.laziness

import org.scalatest.{FlatSpec, Matchers}

class StreamSpec extends FlatSpec with Matchers {

  behavior of "Exercise 5.1"

  "Stream.toList" should "evaluate entire Stream and convert it to list" in {
    Stream(1,2,3,4).toList shouldBe List(1,2,3,4)
  }

  "Stream.toList" should "return an empty list for an empty stream" in {
    Stream().toList shouldBe List()
  }

}
