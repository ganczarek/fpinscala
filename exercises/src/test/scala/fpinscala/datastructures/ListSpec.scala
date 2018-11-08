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
    it("tail should do nothing if empty list") {
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

}
