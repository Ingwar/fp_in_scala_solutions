package io.github.Ingwar.fpinscala.laziness

import scala.annotation.tailrec

sealed trait Stream[+A] {

  import Stream._

  def toList: List[A] = this match {
    case Empty => List.empty
    case Cons(head, tail) => head() :: tail().toList
  }

  def take(n: Int): Stream[A] = this match {
    case Empty => empty
    case _ if n <= 0 => empty
    case Cons(head, tail) => cons(head(), tail() take (n - 1))
  }

  def drop(n: Int): Stream[A] = {

    @tailrec
    def loop(stream: Stream[A], i: Int): Stream[A] = stream match {
      case Empty => empty
      case _ if i <= 0 => stream
      case Cons(_, tail) => loop(tail(), i -1)
    }

    loop(this, n)
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(head, tail) if p(head()) => cons(head(), tail() takeWhile p)
    case _ => empty
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(head, tail) => f(head(), tail().foldRight(z){f})
    case _ => z
  }

  def exists(p: A => Boolean): Boolean = foldRight(false) {(a, b) => p(a) || b}

  def forAll(p: A => Boolean): Boolean = foldRight(true) {(a, b) => p(a) && b}

  def takeWhileViAFoldRight(p: A => Boolean): Stream[A] = foldRight(empty[A]) {(a, b) =>
    if (p(a)) cons(a, b) else empty
  }

  def headOption: Option[A] = foldRight(None: Option[A]) {(a, _) =>
    Some(a)
  }

  def map[B](f: A => B): Stream[B] = foldRight(empty[B]) {(a, b) =>
    cons(f(a), b)
  }

  def filter(p: A => Boolean): Stream[A] = foldRight(empty[A]) {(a, b) =>
    if (p(a)) cons(a, b) else b
  }

  def append[AA >: A](xs: => Stream[AA]): Stream[AA] = foldRight(xs) {(a, b) =>
    cons(a, b)
  }

  def flatMap[B](f: A => Stream[B]): Stream[B] = foldRight(empty[B]) {(a, b) =>
    f(a) append  b
  }

  def mapViaUnfold[B](f: A => B): Stream[B] = unfold(this) {
    case Cons(head, tail) => Some(f(head()) -> tail())
    case Empty => None
  }

  def takeViAUnfold(n: Int): Stream[A] = unfold(this -> n) {
    case (Cons(head, tail), i) if i > 0 => Some(head() -> (tail(), i - 1))
    case _ => None
  }

  def takeWhileViUnfold(p: A => Boolean): Stream[A] = unfold(this) {
    case Cons(head, tail) if p(head()) => Some(head() -> tail())
    case _ => None
  }

  def zipWith[B, C](bs: Stream[B])(f: (A, B) => C): Stream[C] = unfold(this -> bs) {
    case (Cons(head1, tail1), Cons(head2, tail2)) => Some(f(head1(), head2()) -> (tail1(), tail2()))
    case _ => None
  }

  def zipAll[B](bs: Stream[B]): Stream[(Option[A], Option[B])] = unfold(this -> bs) {
    case (Cons(head1, tail1), Cons(head2, tail2)) => Some((Some(head1()) -> Some(head2()), tail1() -> tail2()))
    case (Empty, Cons(head, tail)) => Some((None -> Some(head()), empty -> tail()))
    case (Cons(head, tail), Empty) => Some((Some(head())-> None, tail() -> empty))
    case _ => None
  }

  def startsWith[AA >: A](s: Stream[AA]): Boolean = {
    val streamWithEqualLengh = zipAll(s) takeWhile {
      case (_, Some(_)) => true
      case _ => false
    }
    streamWithEqualLengh forAll {case(a, b) => a == b}
  }

  def tails: Stream[Stream[A]] = unfold(this, false) {
    case (s @ Cons(_, tail), finished) if !finished => Some((s, tail() -> false))
    case (Empty, finished) if !finished => Some((empty, empty -> true))
    case _ => None
  }

  def hasSubsequence[AA >: A](s: Stream[AA]): Boolean = tails exists {_ startsWith s}

  def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] = foldRight(Stream(z)) {case (a, s @ Cons(head, _)) =>
    cons(f(a, head()), s)
  }

}

case object Empty extends Stream[Nothing]
case class Cons[+A](head: () => A, tail: () => Stream[A]) extends Stream[A]

object Stream {

  def cons[A](head: => A, tail: => Stream[A]): Stream[A] = {
    lazy val h = head
    lazy val t = tail
    Cons(() => h, () => t)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] = if (as.isEmpty) {
    empty
  } else {
    cons(as.head, apply(as.tail: _*))
  }

  def constant[A](a: A): Stream[A] = cons(a, constant(a))

  def from(n: Int): Stream[Int] = cons(n, from(n + 1))

  def fibs: Stream[Int] = {

    def loop(a: Int, b: Int): Stream[Int] = {
      cons(a, loop(b, a + b))
    }

    loop(0, 1)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case Some((value, nextState)) => cons(value, unfold(nextState) {f})
    case None => empty
  }

  def onesViaUnfold: Stream[Int] = unfold(1) {_ => Some(1 -> 1)}

  def constantViaUnfold[A](a: A): Stream[A] = unfold(a) {_ => Some(a -> a)}

  def fromViaUnfold(n: Int): Stream[Int] = unfold(n) {i => Some(i -> (i + 1))}

  def fibsViaUnfold: Stream[Int] = unfold(0 -> 1) { case(a, b) => Some(a -> (b, a + b))}

}
