package fpinscala.parallelism

import java.util.concurrent.{ExecutorService, Executors}

import org.scalatest.{FlatSpec, Matchers}
import fpinscala.parallelism.{Nonblocking => NB}


class NonblockingSpec extends FlatSpec with Matchers {

  val es: ExecutorService = Executors.newFixedThreadPool(2)

  behavior of "Exercise 7.11"

  "Par.choiceN" should "allow to choose parallel computation from choices" in {
    NB.Par.choiceN(NB.Par.unit(1))(List(NB.Par.unit("a"), NB.Par.unit(1)))(es)(_ shouldBe 1)
    NB.Par.choiceN(NB.Par.unit(0))(List(NB.Par.unit("a"), NB.Par.unit(1)))(es)(_ shouldBe "a")
  }

  "Par.choiceViaChoiceN" should "allow to choose between two parallel computations" in {
    NB.Par.choiceViaChoiceN(NB.Par.unit(true))(NB.Par.unit("ifTrue"), NB.Par.unit("ifFalse"))(es)(_ shouldBe "ifFalse")
    NB.Par.choiceViaChoiceN(NB.Par.unit(false))(NB.Par.unit("ifTrue"), NB.Par.unit("ifFalse"))(es)(_ shouldBe "ifTrue")
  }

}
