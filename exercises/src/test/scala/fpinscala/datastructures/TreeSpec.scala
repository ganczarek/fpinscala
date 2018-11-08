package fpinscala.datastructures

import fpinscala.datastructures.Tree.{size => treeSize}
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

}
