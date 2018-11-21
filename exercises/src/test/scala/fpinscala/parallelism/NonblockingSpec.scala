package fpinscala.parallelism

import java.util.concurrent.{ExecutorService, Executors}

import fpinscala.parallelism.{Nonblocking => NB}
import org.scalatest.{FlatSpec, Matchers}


class NonblockingSpec extends FlatSpec with Matchers {

  val es: ExecutorService = Executors.newFixedThreadPool(2)

  behavior of "Exercise 7.11"

  "Par.choiceN" should "allow to choose parallel computation from choices" in {
    val computations = List(NB.Par.unit("a"), NB.Par.unit(1))
    NB.Par.choiceN(NB.Par.unit(1))(computations)(es)(_ shouldBe 1)
    NB.Par.choiceN(NB.Par.unit(0))(computations)(es)(_ shouldBe "a")
  }

  "Par.choiceViaChoiceN" should "allow to choose between two parallel computations" in {
    NB.Par.choiceViaChoiceN(NB.Par.unit(true))(NB.Par.unit("ifTrue"), NB.Par.unit("ifFalse"))(es)(_ shouldBe "ifFalse")
    NB.Par.choiceViaChoiceN(NB.Par.unit(false))(NB.Par.unit("ifTrue"), NB.Par.unit("ifFalse"))(es)(_ shouldBe "ifTrue")
  }

  "Par.choiceN" should "should throw an exception (for the sake of exercise) when no given index in the list" in {
    val computations = List(NB.Par.unit("a"), NB.Par.unit(1))
    an[IndexOutOfBoundsException] should be thrownBy NB.Par.choiceN(NB.Par.unit(10))(computations)(es)(_ => ())
  }

  behavior of "Exercise 7.12"

  "Par.choiceMap" should "allow to choose from map of parallel computations" in {
    val computations = Map("a" -> NB.Par.unit("ifA"), "b" -> NB.Par.unit("ifB"))
    NB.Par.choiceMap(NB.Par.unit("a"))(computations)(es)(_ shouldBe "ifA")
    NB.Par.choiceMap(NB.Par.unit("b"))(computations)(es)(_ shouldBe "ifB")
  }

  "Par.choiceMap" should "should throw an exception (for the sake of exercise) when no given key in the map" in {
    val computations = Map("a" -> NB.Par.unit("ifA"), "b" -> NB.Par.unit("ifB"))
    an[NoSuchElementException] should be thrownBy NB.Par.choiceMap(NB.Par.unit("c"))(computations)(es)(_ => ())
  }

  behavior of "Exercise 7.13"

  "Par.chooser" should "allow to choose a parallel computation" in {
    NB.Par.chooser(NB.Par.unit(2))(a => NB.Par.unit("a" * a))(es)(_ shouldBe "aa")
  }

  "Par.choiceViaChooser" should " allow to choose between two parallel computations" in {
    NB.Par.choiceViaChooser(NB.Par.unit(true))(NB.Par.unit("ifTrue"), NB.Par.unit("ifFalse"))(es)(_ shouldBe "ifFalse")
    NB.Par.choiceViaChooser(NB.Par.unit(false))(NB.Par.unit("ifTrue"), NB.Par.unit("ifFalse"))(es)(_ shouldBe "ifTrue")
  }

  "Par.choiceNChooser" should "allow to choose parallel computation from given choices" in {
    val computations = List(NB.Par.unit("a"), NB.Par.unit(1))
    NB.Par.choiceNChooser(NB.Par.unit(1))(computations)(es)(_ shouldBe 1)
    NB.Par.choiceNChooser(NB.Par.unit(0))(computations)(es)(_ shouldBe "a")
  }

  behavior of "Exercise 7.14"

  "Par.join" should "flatten nested computations" in {
    NB.Par.join(NB.Par.unit(NB.Par.unit(10)))(es)(_ shouldBe 10)
  }

  "Par.joinViaFlatMap" should "flatten nested computations" in {
    NB.Par.joinViaFlatMap(NB.Par.unit(NB.Par.unit(10)))(es)(_ shouldBe 10)
  }

  "Par.flatMapViaJoin" should "flatten nested computations" in {
    NB.Par.flatMapViaJoin(NB.Par.unit(2))(x => NB.Par.unit(10 * x))(es)(_ shouldBe 20)
  }


}
