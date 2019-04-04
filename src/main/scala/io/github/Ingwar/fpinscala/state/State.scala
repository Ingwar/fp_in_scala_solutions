package io.github.Ingwar.fpinscala.state

case class State[S, +A](run: S => (A, S)) {

  import State._

  def map[B](f: A => B): State[S, B] = flatMap {a => unit(f(a))}

  def flatMap[B](f: A => State[S, B]): State[S, B] = State { s =>
    val (newA, newState) = run(s)
    f(newA).run(newState)
  }

  def map2[B, C](b: State[S, B])(f: (A, B) => C): State[S, C] = flatMap {a => b map {f(a, _)} }

}

object State {

  def unit[S, A](a: A): State[S, A] = State(s => (a, s))

  def sequence[S, A](l: List[State[S, A]]): State[S, List[A]] = {
    l.foldRight(unit[S, List[A]](List.empty)) { (s, acc) =>
      s.map2(acc) {_ :: _}
    }
  }

  def get[S]: State[S, S] = State(s => (s, s))

  def set[S](s: S): State[S, Unit] = State(_ => ((), s))

  def modify[S](f: S => S): State[S, Unit] = for {
    s <- get
    _  <- set(f(s))
  } yield ()

}