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

    ST.runST(p) shouldBe("A", "b", "default")
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

  behavior of "Exercise 14.3"

  "STMap" should "allow to read a value" in {
    ST.runST(new RunnableST[String] {
      override def apply[S]: ST[S, String] =
        for {
          x <- STMap(Map(1 -> "A", 2 -> "B", 3 -> "C"))
          b <- x(2)
        } yield b
    }) shouldBe "B"
  }

  "STMap" should "allow to get optional value" in {
    ST.runST(new RunnableST[(Option[String], Option[String])] {
      override def apply[S]: ST[S, (Option[String], Option[String])] =
      for {
        x <- STMap(Map(1 -> "A", 2 -> "B", 3 -> "C"))
        b <- x.get(2)
        d <- x.get(20)
      } yield (b, d)
    }) shouldBe(Some("B"), None)
  }

  "STMap" should "allow to write a value" in {
    ST.runST(new RunnableST[String] {
      override def apply[S]: ST[S, String] =
        for {
          x <- STMap(Map(1 -> "A", 2 -> "B", 3 -> "C"))
          _ <- x += (10, "Y")
          y <- x(10)
        } yield y
    }) shouldBe "Y"
  }

  "STMap" should "allow to freeze the map" in {
    ST.runST(new RunnableST[Map[Int, String]] {
      override def apply[S]: ST[S, Map[Int, String]] =
        for {
          x <- STMap(Map(1 -> "A", 2 -> "B", 3 -> "C"))
          _ <- x += (10, "Y")
          y <- x.freeze
        } yield y
    }) shouldBe Map(1 -> "A", 2 -> "B", 3 -> "C", 10 -> "Y")
  }

  "STMap" should "allow to remove a value" in {
    ST.runST(new RunnableST[Map[Int, String]] {
      override def apply[S]: ST[S, Map[Int, String]] =
        for {
          x <- STMap(Map(1 -> "A", 2 -> "B", 3 -> "C"))
          _ <- x -= 2
          y <- x.freeze
        } yield y
    }) shouldBe Map(1 -> "A", 3 -> "C")
  }
}