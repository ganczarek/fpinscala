package fpinscala.parallelism

import java.util.concurrent.{ExecutorService, Executors}

import org.scalatest.{FlatSpec, Matchers}

class ParSpec extends FlatSpec with Matchers {

  val es: ExecutorService = Executors.newFixedThreadPool(1)

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


}
