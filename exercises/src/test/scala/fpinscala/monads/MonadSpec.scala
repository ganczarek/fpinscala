package fpinscala.monads

import org.scalatest.{FunSpec, Matchers}

class MonadSpec extends FunSpec with Matchers {

  describe("Option monad") {
    it("should map two Options with map2, both Some") {
      Monad.optionMonad.map2(Some(1), Some("s"))((a: Int, b: String) => a.toString + b) shouldBe Option("1s")
    }

    it("should map two Options with map2, one is None") {
      Monad.optionMonad.map2(None, Some("s"))((a: Int, b: String) => a.toString + b) shouldBe None
      Monad.optionMonad.map2(Some(1), None)((a: Int, b: String) => a.toString + b) shouldBe None
    }

    it("should map two Options with map2, both None") {
      Monad.optionMonad.map2(None, None)((a: Int, b: String) => a.toString + b) shouldBe None
    }
  }

  describe("Monadic combinators") {
    it("should traverse") {
      def unit(x: Any) = Monad.optionMonad.unit(x)
      val list = List(1, 2, 3, 4)
      Monad.optionMonad.traverse(list)(unit(_)) shouldBe unit(list)
    }

    it("should traverse list monad") {
      def unit(x: Any) = Monad.listMonad.unit(x)
      val list = List(1, 2)
      Monad.listMonad.traverse(list)(a => List(a, a)) shouldBe List(list, list, list, list)
    }

    it("should combine list of monad instances into list monad") {
      def unit(x: Any) = Monad.optionMonad.unit(x)
      val list = List(unit(1), unit(2), unit(3), unit(4))
      Monad.optionMonad.sequence(list) shouldBe unit(List(1, 2, 3, 4))
    }

    it("should combine lists of more than one element") {
      def unit(x: Any) = Monad.listMonad.unit(x)
      val list = List(List(1, 2), List(2, 3))
      Monad.listMonad.sequence(list) should contain theSameElementsAs List(List(1, 2), List(1, 3), List(2, 2), List(2, 3))
    }

    it("should replicate in option monad") {
      def unit(x: Any) = Monad.optionMonad.unit(x)
      Monad.optionMonad.replicateM(5, unit(1)) shouldBe Some(List(1, 1, 1, 1, 1))
      Monad.optionMonad.replicateM(5, unit(null)) shouldBe None
    }

    it("should replicate in list monad") {
      def unit(x: Any) = Monad.listMonad.unit(x)
      Monad.listMonad.replicateM(5, unit(1)) shouldBe List(List(1, 1, 1, 1, 1))
      Monad.listMonad.replicateM(1, List(1, 2)) shouldBe List(List(1), List(2))
      Monad.listMonad.replicateM(2, List(1, 2)) should contain theSameElementsAs List(List(1, 1), List(1, 2), List(2, 1), List(2, 2))
    }

    it("should filterM a list monad") {
      val list = List(1, 2, 3, 4)
      Monad.listMonad.filterM(list)(x => List(x > 2)) shouldBe List(List(3, 4))
    }

    it("should filterM a option monad") {
      val list = List(1, 2, 3, 4)
      Monad.optionMonad.filterM(list)(x => Some(x > 2)) shouldBe Some(List(3, 4))
    }
  }

}
