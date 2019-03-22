package io.github.Ingwar.fpinscala.gettingstarted

import scala.annotation.tailrec

object Polymorphic {

  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {

    @tailrec
    def loop(n: Int): Boolean = n match {
      case _ if as.length <= 1 => true
      case _ if n >= as.length - 2 => ordered(as(n), as(n + 1))
      case _ if !ordered(as(n), as(n + 1)) => false
      case i => loop(i + 1)
    }

    loop(0)
  }

}
