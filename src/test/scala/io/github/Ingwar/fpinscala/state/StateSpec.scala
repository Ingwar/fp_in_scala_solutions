package io.github.Ingwar.fpinscala.state

import org.scalatest.{Inside, Matchers, WordSpec}

class StateSpec extends WordSpec with Matchers with Inside {

  import State._

  type StringifiedCounter = State[Int, String]

  def counter: StringifiedCounter = State(c => (c.toString, c + 1))

  "State#map()" should {

    "transform result of state action by applying given function" in {
      val formatedCounter = counter map {s => "Current invocation count is " + s}
      inside(formatedCounter run 0) { case (message, count) =>
        message shouldBe "Current invocation count is 0"
        count shouldBe 1
      }
    }

  }

  "State#flatMap()" should {

    "transform result of state action by applying given function" in {
      val duplicateIfEvenNumberOfDigits = counter flatMap {x => if (x.length % 2 == 0) unit(x + x) else unit(x)}
      inside(duplicateIfEvenNumberOfDigits run 10) { case (s, count) =>
        s shouldBe "1010"
        count shouldBe 11
      }
    }

  }

  "State#map2()" should {

    "transform result of this and argument stateful actions by applying provided function" in {
      val doubleCounter: State[Int, Double] = State(c => (c.toDouble, c + 1))
      val tupleCounter = counter.map2(doubleCounter) {(a, b) => a -> b}
      inside(tupleCounter run 0) { case ((s, d), count) =>
          s shouldBe "0"
          d shouldBe 1.0
          count shouldBe 2
      }
    }

  }

  "State#unit()" should {

    "create state action that will pass state along unchanged and returns provided value" in {
      val a = unit[Int, String]("a")
      inside(a run 12) { case (newValue, newState) =>
        newState shouldBe 12
        newValue shouldBe "a"
      }
    }

  }

  "State#sequence()" should {

    "merge list of stateful actions to one returning the list of results" in {
      val counters = sequence(List.fill(3)(counter))
      inside(counters run 0) { case (l, count) =>
        l shouldBe List("0", "1", "2")
        count shouldBe 3
      }
    }

  }


  private def $return = afterWord("return")

}
