package io.github.Ingwar.fpinscala.gettingstarted

import org.scalatest.{FlatSpec, Matchers}

class PolymorphicSpec extends FlatSpec with Matchers {

  import Polymorphic._

  private def ascInt(a: Int, b: Int) = a <= b

  private def descInt(a: Int, b: Int) = a >= b

  private def descString(a: String, b: String) = a >= b

  "Polymorphic#isSorted" should "return true for integers sorted in ascending order" in {
    isSorted(Array(1, 2, 3, 4), ascInt) should be (true)
  }

  it should "return true for integers sorted in descending order" in {
    isSorted(Array(4, 3, 2, 1), descInt) should be (true)
  }

  it should "return true for equal strings" in {
    isSorted(Array("aa", "aa", "aa"), descString) should be (true)
  }

  it should "return false for non-sorted integers" in {
    isSorted(Array(1, 2, 3, 2, 3), ascInt) should be (false)
  }

  it should "return true for empty array" in {
    isSorted(Array(), ascInt) should be (true)
  }

  it should "return true for one-element array" in {
    isSorted(Array(1), ascInt) should be (true)
  }

}

