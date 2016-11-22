package fpinscala.monoids

import org.scalatest.{FunSpec, Matchers}

class FoldMonoidsSpec extends FunSpec with Matchers {

  it("should fold a list of string representation of Ints with intAddition monoid") {
    val testList = List(1,2,3,4,5,6,7).map(_.toString)
    Monoid.foldMap[String,Int](testList, Monoid.intAddition)(_.toInt) shouldBe testList.size*(testList.size+1)/2
  }

}
