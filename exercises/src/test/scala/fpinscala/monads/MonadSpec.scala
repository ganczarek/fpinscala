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

}
