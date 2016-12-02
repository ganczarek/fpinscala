package fpinscala.monoids

import org.scalatest.{FunSpec, Matchers}

class FoldMonoidsSpec extends FunSpec with Matchers {

  describe("foleMap") {
    it("should fold a list of string representation of Ints with intAddition monoid") {
      val testList = (1 to 7).map(_.toString).toList
      Monoid.foldMap(testList, Monoid.intAddition)(_.toInt) shouldBe testList.size * (testList.size + 1) / 2
    }
  }

  describe("foldLeft") {
    it("should fold left list of integers") {
      val testList = (1 to 7).toList
      val monoid = Monoid.intMultiplication
      Monoid.foldLeft(testList)(monoid.zero)(monoid.op) shouldBe 2 * 3 * 4 * 5 * 6 * 7
    }

    it("should fold left list of strings") {
      val testList = List("a", "b", "c", "d")
      Monoid.foldLeft(testList)("")((s1: String, s2: String) => s1.concat(s2)) shouldBe "abcd"
    }
  }

  describe("foldRight") {
    it("should fold right list of integers") {
      val testList = (1 to 7).toList
      val monoid = Monoid.intMultiplication
      Monoid.foldRight(testList)(monoid.zero)(monoid.op) shouldBe 2 * 3 * 4 * 5 * 6 * 7
    }

    it("should fold right list of strings") {
      val testList = List("a", "b", "c", "d")
      Monoid.foldRight(testList)("")((s1: String, s2: String) => s1.concat(s2)) shouldBe "abcd"
    }
  }

  describe("foldMap with balanced fold") {
    it("should fold empty sequence with identity mapping to zero value of the monoid") {
      val testSeq = IndexedSeq()
      Monoid.foldMapV(testSeq, Monoid.intMultiplication)(Predef.identity) shouldBe Monoid.intMultiplication.zero
    }

    it("should fold sequence with single element") {
      val testSeq = IndexedSeq("1")
      Monoid.foldMapV(testSeq, Monoid.intMultiplication)(_.toInt) shouldBe 1
    }

    it("should fold sequence with even number of elements") {
      val testSeq = (1 to 8).map(_.toString)
      Monoid.foldMapV(testSeq, Monoid.intAddition)(_.toInt) shouldBe (1 + testSeq.size) * testSeq.size / 2
    }

    it("should fold sequence with odd number of elements") {
      val testSeq = (1 to 17).map(_.toString)
      Monoid.foldMapV(testSeq, Monoid.intAddition)(_.toInt) shouldBe (1 + testSeq.size) * testSeq.size / 2
    }
  }

}
