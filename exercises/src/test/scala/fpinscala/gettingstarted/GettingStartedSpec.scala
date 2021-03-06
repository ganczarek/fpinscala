package fpinscala.gettingstarted

import org.scalatest.{FunSpec, Matchers}

import fpinscala.gettingstarted.MyModule.fib
import fpinscala.gettingstarted.PolymorphicFunctions.isSorted
import fpinscala.gettingstarted.PolymorphicFunctions.{compose, composeInScalaWay}
import fpinscala.gettingstarted.PolymorphicFunctions.{curry, curryInScalaWay, uncurry, uncurryInScalaWay}


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

  describe("IsSorted function") {

    val descIntCompFunc = (x:Int, y:Int) => x >= y

    it("array is sorted when empty") {
      isSorted(Array(), descIntCompFunc) shouldBe true
    }

    it("array is sorted when only a single item") {
      isSorted(Array(1), descIntCompFunc) shouldBe true
    }

    it("should be sorted when all elements are the same") {
      isSorted(Array(5,5,5,5,5), descIntCompFunc) shouldBe true
    }

    it("should support array of integers when sorted") {
      isSorted(Array(4,3,2,1), descIntCompFunc) shouldBe true
      isSorted(Array(12,4,1), descIntCompFunc) shouldBe true
    }

    it("should support array of integers when not sorted") {
      isSorted(Array(1,2,3,4), descIntCompFunc) shouldBe false // ascending order
      isSorted(Array(15, -2, 12), descIntCompFunc) shouldBe false // shuffled
      isSorted(Array(4,5,3,2,1), descIntCompFunc) shouldBe false // first two values out of order
      isSorted(Array(5,4,3,1,2), descIntCompFunc) shouldBe false // last two values out of order
    }

    it("should support array of other types") {
      isSorted(Array("A", "B", "C", "D"), (x:String, y:String) => x <= y) shouldBe true
      isSorted(Array("A", "B", "C", "D"), (x:String, y:String) => x > y) shouldBe false
      isSorted(Array(1.0, 2.0, 3.0), (x:Double, y:Double) => x < y) shouldBe true
      isSorted(Array(1.0, 2.0, 3.0), (x:Double, y:Double) => x > y) shouldBe false
    }
  }

  describe("curry function") {
    it("should allow to curry one argument of two arguments function") {
      val func = (a:Int, b:Int) => a * b
      val curriedFunc = curry(func)(10)
      curriedFunc(10) shouldBe 100
      curriedFunc(5) shouldBe 50
    }

    it("scala way of currying a function") {
      val func = (a:Int, b:Int) => a * b
      val curriedFunc = curryInScalaWay(func)(10)
      curriedFunc(10) shouldBe 100
      curriedFunc(5) shouldBe 50
    }
  }

  describe("uncurry function") {
    it("should allow to uncurry a function") {
      val func = (a:Int, b:Int) => a * b
      val curriedFunc = func.curried
      val uncurriedFunc = uncurry(curriedFunc)

      for{
        i <- 0 until 10
        j <- 0 until 10
      } uncurriedFunc(i,j) shouldBe func(i,j)
    }

    it("scala way to uncurry a function") {
      val func = (a:Int, b:Int) => a * b
      val curriedFunc = func.curried
      val uncurriedFunc = uncurryInScalaWay(curriedFunc)

      for{
        i <- 0 until 10
        j <- 0 until 10
      } uncurriedFunc(i,j) shouldBe func(i,j)

    }
  }

  describe("compose functions") {
    it("should compose two functions") {
      val doubleToInt = (x:Double) => x.toInt
      val intToString = (x:Int) => x.toString

      compose(intToString, doubleToInt)(10.23) shouldBe "10"
      compose(intToString, doubleToInt)(5.0) shouldBe "5"
    }

    it("scala way to compose two functions") {
      val doubleToInt = (x:Double) => x.toInt
      val intToString = (x:Int) => x.toString

      composeInScalaWay(intToString, doubleToInt)(10.23) shouldBe "10"
      composeInScalaWay(intToString, doubleToInt)(5.0) shouldBe "5"
    }
  }
}