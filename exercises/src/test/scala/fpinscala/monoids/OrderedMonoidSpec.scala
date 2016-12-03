package fpinscala.monoids

import org.scalatest.{FunSpec, Matchers}

class OrderedMonoidSpec extends FunSpec with Matchers {

  it("should detect that empty sequence is ordered") {
    Monoid.ordered(IndexedSeq()) shouldBe true
  }

  it("should detect that sequence of one element is ordered") {
    Monoid.ordered(IndexedSeq(1)) shouldBe true
  }

  it("should detect that ascending sequence of many elements is ordered") {
    Monoid.ordered(IndexedSeq(-5, 0, 5, 10)) shouldBe true
  }

  it("should detect that descending sequence of many elements is ordered") {
    Monoid.ordered(IndexedSeq(10, 5, 0, -5)) shouldBe true
  }

  it("should detect that sequence of same elements is ordered") {
    Monoid.ordered(IndexedSeq(1, 1, 1, 1, 1, 1, 1, 1, 1, 1)) shouldBe true
  }

  it("should detect that sequence is not ordered") {
    Monoid.ordered(IndexedSeq(1, 2, 5, 3, 4)) shouldBe false
  }
}
