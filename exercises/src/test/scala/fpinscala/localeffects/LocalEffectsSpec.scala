package fpinscala.localeffects

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

}
