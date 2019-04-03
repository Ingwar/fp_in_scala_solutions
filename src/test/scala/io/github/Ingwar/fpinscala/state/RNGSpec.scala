package io.github.Ingwar.fpinscala.state

import org.scalatest.{Matchers, WordSpec}

import scala.annotation.tailrec

class RNGSpec extends WordSpec with Matchers {

  import RNG._
  import RNGSpec._

  "RNG#nonNegativeInt()" should $return {

    "values between 0 and Int.MaxValue (inclusive)" in {
      all(sample(100) {nonNegativeInt}) should (be >= 0 and be <= Int.MaxValue)
    }

    "0 when underlying RNG returns Int.MinValue" in {
      val (result, _) = nonNegativeInt(MinIntRng)
      result shouldBe 0
    }

  }

  "RNG#double()" should $return {

    "values between 0 (inclusive) and 1 (exclusive)" in {
      all(sample(100) {double}) should (be >= 0.0 and be < 1.0)
    }

    "0 when underlying RNG returns 0" in {
      val (result, _) = double(ZeroRng)
      result shouldBe 0.0
    }

    "value less than 1 when underlying RNG returns Int.MaxValue" in {
      val (result, _) = double(MaxIntRng)
      result should be < 1.0
    }

  }

  "RNG#intDouble()" should $return {

    "pair of int and double where double is not a mapping of int" in {
      val ((i, d), _) = intDouble(SimpleRng(0))
      d shouldNot be (i / (Int.MaxValue.toDouble + 1))
    }

    "distinct results on calls with different RNGs" in {
      val results = sample(100) {intDouble}
      results.toSet should have size 100
    }

  }


  "RNG#doubleInt()" should $return {

    "pair of double and int where double is not a mapping of int" in {
      val ((d, i), _) = doubleInt(SimpleRng(0))
      d shouldNot be (i / (Int.MaxValue.toDouble + 1))
    }

    "distinct results on calls with different RNGs" in {
      val results = sample(100) {doubleInt}
      results.toSet should have size 100
    }

  }

  "RNG#double3()" should $return {

    "pair of distinct doubles" in {
      val ((d1, d2, d3), _) = double3(SimpleRng(0))
      d1 shouldNot be (d2 +- tolerance)
      d2 shouldNot be (d3 +- tolerance)
    }

    "distinct results on calls with different RNGs" in {
      val results = sample(100) {double3}
      results.toSet should have size 100
    }

  }

  "RNG#ints()" should $return {

    "list of given size" in {
      val (l, _) = ints(100)(SimpleRng(0))
      l should have size 100
    }

    "list of distinct random numbers" in {
      val (l, _) = ints(100)(SimpleRng(0))
      l.toSet should have size 100
    }

  }

  "RNG#doubleViaMap()" should $return {

    "values between 0 (inclusive) and 1 (exclusive)" in {
      all(sample(100) {doubleViaMap}) should (be >= 0.0 and be < 1.0)
    }

    "0 when underlying RNG returns 0" in {
      val (result, _) = doubleViaMap(ZeroRng)
      result shouldBe 0.0
    }

    "value less than 1 when underlying RNG returns Int.MaxValue" in {
      val (result, _) = doubleViaMap(MaxIntRng)
      result should be < 1.0
    }

  }

  "RNG#randIntDouble()" should $return {

    "pair of int and double where double is not a mapping of int" in {
      val ((i, d), _) = randIntDouble(SimpleRng(0))
      d shouldNot be (i / (Int.MaxValue.toDouble + 1))
    }

    "distinct results on calls with different RNGs" in {
      val results = sample(100) {randIntDouble}
      results.toSet should have size 100
    }

  }

  "RNG#randDoubleInt()" should $return {

    "pair of double and int where double is not a mapping of int" in {
      val ((d, i), _) = randDoubleInt(SimpleRng(0))
      d shouldNot be (i / (Int.MaxValue.toDouble + 1))
    }

    "distinct results on calls with different RNGs" in {
      val results = sample(100) {randDoubleInt}
      results.toSet should have size 100
    }

  }

  "RNG#intsViaSequence()" should $return {

    "list of given size" in {
      val (l, _) = intsViaSequence(100)(SimpleRng(0))
      l should have size 100
    }

    "list of distinct random numbers" in {
      val (l, _) = intsViaSequence(100)(SimpleRng(0))
      l.toSet should have size 100
    }

  }

  private def $return = afterWord("return")

  private val tolerance = 1e-8
}

object RNGSpec {

  private def sample[A](n: Int, initialSeed: Long = 0)(f: RNG => (A, RNG)) = {

    @tailrec
    def loop(i: Int, rng: RNG, acc: List[A]): List[A] = if (i <= 0) {
      acc
    } else {
      val (newVal, newRng) = f(rng)
      loop(i - 1, newRng, newVal :: acc)
    }

    loop(n, SimpleRng(initialSeed), Nil)
  }

  object MinIntRng extends RNG {

    override def nextInt: (Int, RNG) = (Int.MinValue, this)

  }

  object MaxIntRng extends RNG {

    override def nextInt: (Int, RNG) = (Int.MaxValue, this)

  }

  object ZeroRng extends RNG {

    override def nextInt: (Int, RNG) = (0, this)

  }

}
