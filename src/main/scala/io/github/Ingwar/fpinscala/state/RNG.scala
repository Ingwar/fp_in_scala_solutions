package io.github.Ingwar.fpinscala.state

import scala.annotation.tailrec

trait RNG {

  def nextInt: (Int, RNG)

}

case class SimpleRng(seed: Long) extends RNG {

  override def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
    val nextRNG = SimpleRng(newSeed) // The next state, which is an `RNG` instance created from the new seed.
    val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
    (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
  }

}

object RNG {

  type Rand[+A] = RNG => (A, RNG)

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (value, nextRng) = rng.nextInt
    value match {
      case Int.MinValue => (0, nextRng)
      case i if i < 0 => (-i, nextRng)
      case i => (i, nextRng)
     }
  }

  def double(rng: RNG): (Double, RNG) = {
    val (int, nextRng) = nonNegativeInt(rng)
    (int / (Int.MaxValue.toDouble + 1), nextRng)
  }

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (int, rng2) = rng.nextInt
    val (d, rng3) = double(rng2)
    ((int, d), rng3)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val (d, rng2) = double(rng)
    val (i, rng3) = rng2.nextInt
    ((d, i), rng3)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, rng2) = double(rng)
    val (d2, rng3) = double(rng2)
    val (d3, rng4) = double(rng3)
    ((d1, d2, d3), rng4)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {

    @tailrec
    def loop(i: Int, r: RNG, acc: List[Int]): (List[Int], RNG) = if (i <= 0) {
      (acc, r)
    } else {
      val (nextValue, nextRng) = r.nextInt
      loop(i - 1, nextRng, nextValue :: acc)
    }

    loop(count, rng, Nil)
  }

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] = rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] = rng => {
    val (a, rng2) = s(rng)
    (f(a), rng2)
  }

  val doubleViaMap: Rand[Double] = map(nonNegativeInt) {_ / (Int.MaxValue.toDouble + 1)}

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = rng => {
    val (a, rng2) = ra(rng)
    val (b, rng3) = rb(rng2)
    (f(a, b), rng3)
  }

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] = map2(ra, rb) {(_, _)}

  val randIntDouble: Rand[(Int, Double)] = both(int, double)

  val randDoubleInt: Rand[(Double, Int)] = both(double, int)

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = rng => {

    fs.foldRight((List.empty[A], rng)) { case (r, (as, s)) =>
      val (nextA, nextS) = r(s)
      (nextA :: as, nextS)
    }

  }

  def intsViaSequence(count: Int): Rand[List[Int]] = sequence(List.fill(count) {int})

  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = rng => {
    val (a, rng2) = f(rng)
    g(a)(rng2)
  }

  def nonNegativeLessThan(n: Int): Rand[Int] = flatMap(nonNegativeInt) { i =>
    val mod = i % n
    if (i + (n - 1) - mod >= 0) {
      unit(mod)
    } else {
      nonNegativeLessThan(n)
    }
  }

  def mapViaFlatMap[A, B](s: Rand[A])(f: A => B): Rand[B] = flatMap(s) {a => unit(f(a))}

  def map2ViaFlatMap[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = flatMap(ra) {a => map(rb) {b => f(a, b)} }

}
