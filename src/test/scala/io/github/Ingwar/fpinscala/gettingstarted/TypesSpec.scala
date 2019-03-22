package io.github.Ingwar.fpinscala.gettingstarted

import org.scalatest.{FlatSpec, Matchers}

class TypesSpec extends FlatSpec with Matchers {

  import Types._

  "curry" should "convert 2-args funtion to one-arg" in {
    val adder = curry((a: Int, b: Int) => a + b)
    val increment = adder(1)
    increment(2) should be (3)
  }

  "uncurry" should "revert effect of curry" in {
    val adder = (a: Int, b: Int) => a + b
    val carriedAdder = curry(adder)
    val uncarriedAdder = uncurry(carriedAdder)
    adder(1, 2) shouldBe 3
    uncarriedAdder(1, 2) shouldBe adder(1, 2)
  }

  "compose" should "apply one function after another" in {
    val stringify = (a: Int) => a.toString
    val countChars = (b: String) => b.length
    val countDigits = compose(countChars, stringify)
    countDigits(111) shouldBe 3
  }

}
