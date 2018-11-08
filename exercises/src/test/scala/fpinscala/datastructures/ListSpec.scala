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

  describe("Exercise 3.4") {
    it("drop should return empty list if a list is already empty") {
      drop(List(), 10) shouldBe List()
    }

    it("drop should return empty list if dropping more object than in the list") {
      drop(List(1, 2, 3), 10) shouldBe List()
    }

    it("drop should do thing if dropping 0 elements") {
      drop(List(1, 2, 3), 0) shouldBe List(1, 2, 3)
    }

    it("drop should remove first n elements of the list") {
      drop(List(1, 2, 3), 1) shouldBe List(2, 3)
      drop(List(1, 2, 3), 2) shouldBe List(3)
    }
  }

  describe("Exercise 3.5") {
    it("dropWhile should return empty list if a list is already empty") {
      dropWhile(List(), (_: Any) => true) shouldBe List()
      dropWhile(List(), (_: Any) => false) shouldBe List()
    }

    it("dropWhile should remove all elements if predicate is always true") {
      dropWhile(List(1, 2, 3), (_: Any) => true) shouldBe List()
    }

    it("dropWhile should halt when predicate is not matched") {
      dropWhile(List(1, 2, 3, 4, 5, 6), (x: Int) => x < 4) shouldBe List(5, 6)
    }

    it("dropWhile when first element matches the predicate") {
      dropWhile(List(1, 2, 3, 4, 5, 6), (x: Int) => x > 4) shouldBe List(1, 2, 3, 4, 5, 6)
    }
  }

  describe("Exercise 3.6") {
    it("init should drop last element of the list") {
      init(List(1, 2, 3, 4, 5)) shouldBe List(1, 2, 3, 4)
    }

    it("init should return an empty list when list has a single element") {
      init(List(1)) shouldBe List()
    }

    it("init should return an empty list when list is already empty") {
      init(List()) shouldBe List()
    }
  }

  describe("Exercise 3.9") {
    import fpinscala.datastructures.List.{length => exerciseLength}

    it("length should return 0 for an empty list") {
      exerciseLength(List()) shouldBe 0
    }

    it("length should return length of a non-empty list") {
      exerciseLength(List(1)) shouldBe 1
      exerciseLength(List(1, 2, 3, 4, 5)) shouldBe 5
    }
  }

  describe("Exercise 3.10") {
    it("foldLeft should work and be (hopefully - not test here) stack-safe") {
      val z = 2
      val op = (a: Int, b: Int) => a * b
      foldLeft(List(1, 2, 3, 4), z)(op) shouldBe Seq(1, 2, 3, 4).foldLeft(z)(op)
    }
  }
}
