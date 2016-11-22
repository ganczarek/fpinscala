package fpinscala.monoids

import org.scalatest.{FunSpec, Matchers}

class FoldMonoidsSpec extends FunSpec with Matchers {

  it("should fold a list of string representation of Ints with intAddition monoid") {
    val testList = (1 to 7).map(_.toString).toList
    Monoid.foldMap(testList, Monoid.intAddition)(_.toInt) shouldBe testList.size*(testList.size+1)/2
  }

  it("should fold left list of integers") {
    val testList = (1 to 7).toList
    val monoid = Monoid.intMultiplication
    Monoid.foldLeft(testList)(monoid.zero)(monoid.op) shouldBe 2*3*4*5*6*7
  }

  it("should fold right list of integers") {
    val testList = (1 to 7).toList
    val monoid = Monoid.intMultiplication
    Monoid.foldRight(testList)(monoid.zero)(monoid.op) shouldBe 2*3*4*5*6*7
  }

  it("should fold left list of strings") {
    val testList = List("a","b","c","d")
    Monoid.foldLeft(testList)("")((s1:String, s2:String) => s1.concat(s2)) shouldBe "abcd"
  }

  it("should fold right list of strings") {
    val testList = List("a","b","c","d")
    Monoid.foldRight(testList)("")((s1:String, s2:String) => s1.concat(s2)) shouldBe "abcd"
  }

}
