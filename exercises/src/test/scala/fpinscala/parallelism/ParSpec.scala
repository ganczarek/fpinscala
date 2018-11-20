package fpinscala.parallelism

import java.util.concurrent.{ExecutorService, Executors}

import org.scalatest.{FlatSpec, Matchers}

class ParSpec extends FlatSpec with Matchers {

  val es: ExecutorService = Executors.newFixedThreadPool(2)

  behavior of "Exercise 7.4"

  "Par.asyncF" should "lazily evaluate" in {
    var count = 0
    val f = (x: Int) => {
      count = count + 1
      x.toString
    }

    val future = Par.asyncF(f)(10)(es)

    count shouldBe 0 // function f should not be evaluated yet
    future.get shouldBe "10"
    count shouldBe 1
  }

  behavior of "Exercise 7.5"

  "Par.sequence" should "combine list of parallel computations into a computation with list of results" in {
    val pars = List(Par.unit(10), Par.lazyUnit(10 + 2), Par.unit(1))

    Par.sequence(pars)(es).get shouldBe List(10, 12, 1)
  }

  behavior of "Exercise 7.6"

  "Par.parFilter" should "filter elements" in {
    Par.parFilter(List(1,6,2,5,3,4))(_ > 3)(es).get shouldBe List(6,5,4)
  }

}
