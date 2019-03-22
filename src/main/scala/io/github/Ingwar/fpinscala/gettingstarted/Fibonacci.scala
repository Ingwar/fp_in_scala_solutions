package io.github.Ingwar.fpinscala.gettingstarted

import scala.annotation.tailrec

object Fibonacci {

  def fib(n: Int): Int = {

    @tailrec
    def loop(i: Int, previousNumber: Int, antepenultimateNumber: Int): Int = i match {
      case _ if i <= 0 => antepenultimateNumber
      case a => loop(a - 1, previousNumber + antepenultimateNumber, previousNumber)
    }
    loop(n, 1, 0)
  }

}
