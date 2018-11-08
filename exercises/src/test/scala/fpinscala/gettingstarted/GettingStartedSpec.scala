package fpinscala.gettingstarted

import org.scalatest.{FunSpec, Matchers}

import fpinscala.gettingstarted.MyModule.fib

class GettingStartedSpec extends FunSpec with Matchers {

  describe("Fibonacci function") {
    it("should return 0 for index 0") {
      fib(0) shouldBe 0
    }

    it("should return 1 for index 1") {
      fib(1) shouldBe 1
    }

    it("should return values for n > 1") {
      fib(2) shouldBe 1
      fib(3) shouldBe 2
      fib(4) shouldBe 3
      fib(5) shouldBe 5
      fib(6) shouldBe 8
    }

    it("should support BigInts without stack overflow") {
      fib(300) shouldBe BigInt("222232244629420445529739893461909967206666939096499764990979600")
    }
  }

}