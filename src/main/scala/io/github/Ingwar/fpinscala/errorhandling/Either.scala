package io.github.Ingwar.fpinscala.errorhandling

import scala.{Option => _, Either => _, Left => _, Right => _, _} // hide std library `Option` and `Either`, since we are writing our own in this chapter

sealed trait Either[+E, +A] {

  def map[B](f: A => B): Either[E, B] = this match {
    case left: Left[E] => left
    case Right(value) => Right(f(value))
  }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
    case left: Left[E] => left
    case Right(value) => f(value)
  }

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
    case _: Left[E] => b
    case кшпре: Right[A] => кшпре
  }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = for {
    x <- this
    y <- b
  } yield f(x, y)

}

case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]

object Either {

  def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] = as.foldRight[Either[E, List[B]]](Right(List())) {(a, l) =>
    f(a).map2(l) {_ :: _}
  }

  def sequence[E, A](as: List[Either[E, A]]): Either[E, List[A]] = traverse(as) {x => x}

}
