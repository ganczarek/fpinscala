package fpinscala.localeffects

import fpinscala.testing.Prop.Passed
import fpinscala.testing.{Gen, Prop}
import org.scalatest.{FlatSpec, Matchers}

class LocalEffectsSpec extends FlatSpec with Matchers {

  behavior of "Exercise 14.1"

  "STArray.fill" should "fill in an array from a Map" in {
    val p = new RunnableST[(String, String, String)] {
      override def apply[S]: ST[S, (String, String, String)] =
        for {
          x <- STArray(10, "default")
          _ <- x.fill(Map(1 -> "A", 5 -> "b"))
          a <- x.read(1)
          b <- x.read(5)
          c <- x.read(4)
        } yield (a, b, c)
    }

    ST.runST(p) shouldBe ("A", "b", "default")
  }

  behavior of "Exercise 14.2"

  "purely functional quicksort" should "sort empty list" in {
    Immutable.quicksort(List()) shouldBe List()
  }

  "purely functional quicksort" should "sort list" in {
    val p = Prop.forAll(Gen.listOf(Gen.choose(-10000, 1000))) {
      l => Immutable.quicksort(l) == l.sorted
    }
    Prop.run(p) shouldBe Passed
  }

  "purely functional quicksort" should "not modify input list" in {
    val list = List(1, 4, 2, 6, 3, 1)

    Immutable.quicksort(list) should not be list
    list shouldBe List(1, 4, 2, 6, 3, 1)
  }
}
