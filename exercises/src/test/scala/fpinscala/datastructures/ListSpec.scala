package fpinscala.datastructures

import fpinscala.datastructures.List._
import org.scalatest.{FunSpec, Matchers}

class ListSpec extends FunSpec with Matchers {

  describe("Exercise 3.1") {
    it("should be 3") {
      import fpinscala.datastructures.List.x
      x shouldBe 3
    }
  }

  describe("Exercise 3.2") {
    it("tail should throw exception when called with an empty list (for the sake of exercise)") {
      assertThrows[IllegalArgumentException] {
        tail(List())
      }
    }

    it("tail should return empty list for a list with a single element") {
      tail(List(1)) shouldBe List()
    }

    it("tail should remove first element form the list") {
      tail(List(1, 2, 3, 4, 5)) shouldBe List(2, 3, 4, 5)
    }
  }

  describe("Exercise 3.3") {
    it("setHead should throw exception when called with an empty list (for the sake of exercise)") {
      assertThrows[IllegalArgumentException] {
        setHead(List(), 1) shouldBe List(1)
      }
    }

    it("setHead should replace the head of the list") {
      setHead(List(1, 2, 3, 4), "12") shouldBe List("12", 2, 3, 4)
    }
  }
}
