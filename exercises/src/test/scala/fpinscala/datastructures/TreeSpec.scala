package fpinscala.datastructures

import fpinscala.datastructures.Tree.{size => treeSize, _}
import org.scalatest.{FunSpec, Matchers}

class TreeSpec extends FunSpec with Matchers {

  val testTree = Branch(
    Leaf(1),
    Branch(
      Branch(
        Leaf(2),
        Leaf(3)
      ),
      Leaf(4)
    )
  )

  describe("Exercise 3.25") {
    it("size should return number of node in the tree") {
      treeSize(testTree) shouldBe 7
    }
  }

  describe("Exercise 3.26") {
    it("maxValue should return max value in a tree") {
      maxValue(testTree) shouldBe 4
    }
  }

  describe("Exercise 3.27") {
    it("depth should return max length of the path from root to any leaf in the tree") {
      depth(testTree) shouldBe 3
    }
  }

  describe("Exercise 3.28") {
    it("map implementation for trees") {
      map(testTree)(_.toString) shouldBe Branch(
        Leaf("1"),
        Branch(
          Branch(
            Leaf("2"),
            Leaf("3")
          ),
          Leaf("4")
        )
      )
    }
  }
}
