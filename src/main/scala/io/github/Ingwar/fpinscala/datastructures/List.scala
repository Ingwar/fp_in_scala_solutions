package io.github.Ingwar.fpinscala.datastructures

import scala.annotation.tailrec

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {

  def apply[A](as: A*): List[A] = if (as.isEmpty) {
    Nil
  } else {
    Cons(as.head, apply(as.tail: _*))
  }

  def tail[A](xs: List[A]): List[A] = xs match {
    case Nil => Nil
    case Cons(_, t) => t
  }

  def setHead[A](xs: List[A], h: A): List[A] = xs match {
    case Nil => throw new Exception("Attempt to set a head of an empty list")
    case Cons(_, t) => Cons(h, t)
  }

  @tailrec
  def drop[A](xs: List[A], n: Int): List[A] = {
    require(n >= 0, s"n should be non-negative but was $n")

    xs match {
      case Nil => Nil
      case l if n == 0 => l
      case Cons(_, t) => drop(t, n -1)
    }
  }

  @tailrec
  def dropWhile[A](xs: List[A], p: A => Boolean): List[A] = xs match {
    case Nil => Nil
    case Cons(h, t) if p(h) => dropWhile(t, p)
    case _ => xs
  }

  def init[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(_, Nil) => Nil
    case Cons(h, t) => Cons(h, init(t))
  }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def length[A](as: List[A]): Int = foldRight(as, 0) {(_, lengthSoFar) => lengthSoFar + 1}

  @tailrec
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
  }

  def sum(ints: List[Int]): Int = foldLeft(ints, 0) {_ + _}

  def product(ints: List[Int]): Int = foldLeft(ints, 1) {_ * _}

  def length2[A](as: List[A]): Int = foldLeft(as, 0) {(lengthSoFar, _) => lengthSoFar + 1}

  def reverse[A](as: List[A]): List[A] = foldLeft(as, Nil: List[A]) {(t, h) => Cons(h, t)}

  def foldLeftViaFoldRight[A, B](as: List[A], z: B)(f: (B, A) => B): B = foldRight(reverse(as), z) {(a, b) => f(b, a)}

  def foldRightViaFoldLeft[A, B](as: List[A], z: B)(f: (A, B) => B): B = foldLeft(reverse(as), z) {(b, a) => f(a, b)}

  def append[A](l1: List[A], l2: List[A]): List[A] = foldRightViaFoldLeft(l1, l2) {Cons(_, _)}

  def concat[A](l: List[List[A]]): List[A] = foldLeft(l, List[A]()) {append}

  def addOne(ints: List[Int]): List[Int] = foldRightViaFoldLeft(ints, List[Int]()) {(h, t) => Cons(h + 1, t)}

  def doublesToStrings(doubles: List[Double]): List[String] = foldRightViaFoldLeft(doubles, List[String]()) {
    (h, t) => Cons(h.toString, t)
  }

  def map[A, B](as: List[A])(f: A => B): List[B] = foldRightViaFoldLeft(as, List[B]()) {(h, t) =>
    Cons(f(h), t)
  }

  def filter[A](as: List[A])(f: A => Boolean): List[A] = foldRightViaFoldLeft(as, List[A]()) {(h, t) =>
    if (f(h)) Cons(h, t) else t
  }

  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] = concat(map(as){f})

  def filterViaFlatMap[A](as: List[A])(p: A => Boolean): List[A] = flatMap(as) {a =>
    if (p(a)) List(a) else List()
  }

  def addLists(xs: List[Int], ys: List[Int]): List[Int] = {

    @tailrec
    def loop(l1: List[Int], l2: List[Int], acc: List[Int]): List[Int] = (l1, l2) match {
      case (Nil, _) => acc
      case(_, Nil) => acc
      case (Cons(h1, t1), Cons(h2, t2)) => loop(t1, t2, Cons(h1 + h2, acc))
    }

    reverse(loop(xs, ys, List()))
  }

  def zipWith[A, B, C](as: List[A], bs: List[B])(f: (A, B) => C): List[C] = {

    @tailrec
    def loop(l1: List[A], l2: List[B], acc: List[C]): List[C] = (l1, l2) match {
      case (Nil, _) => acc
      case(_, Nil) => acc
      case (Cons(h1, t1), Cons(h2, t2)) => loop(t1, t2, Cons(f(h1, h2), acc))
    }

    reverse(loop(as, bs, List()))
  }

  def hasSubSequence[A](sup: List[A], sub: List[A]): Boolean = {
    @tailrec
    def startsWithPrefix(xs: List[A], ys: List[A]): Boolean = (xs, ys) match {
      case (_, Nil) => true
      case (Cons(h1, t1), Cons(h2, t2)) if h1 == h2 => startsWithPrefix(t1, t2)
      case _ => false
    }

    @tailrec
    def loop(as: List[A]): Boolean = as match {
      case Nil => sub == Nil
      case _ if startsWithPrefix(as, sub) => true
      case Cons(_, t) => loop(t)
    }

    loop(sup)
  }

}
