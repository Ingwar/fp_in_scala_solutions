package io.github.Ingwar.fpinscala.gettingstarted

import org.scalatest.{FlatSpec, Matchers}

class FibonacciSpec extends FlatSpec with Matchers {

  import Fibonacci._

  "Fibonacci#fib" should "return corresponding Fibonacci number" in {
    val expectedNumbers = Seq(0, 1, 1, 2, 3, 5, 8)
    val actualNumbers = expectedNumbers.indices map fib
    actualNumbers should contain theSameElementsInOrderAs expectedNumbers
  }

}
