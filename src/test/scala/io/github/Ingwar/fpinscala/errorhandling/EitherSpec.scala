package io.github.Ingwar.fpinscala.errorhandling

import org.scalatest.{Matchers, WordSpec}

class EitherSpec extends WordSpec with Matchers {

  import Either._

  "Either#map()" should {

    "return result of applying function to value when this is a Right" in {
      Right(1234) map {_.toString} shouldBe Right("1234")
    }

    "return this unchanged when it's a Left" in {
      Left("error") map {"Prefix" + _} shouldBe Left("error")
    }

  }

  "Either#flatMap()" should {

    "return function result when it's a Right and this is a Right" in {
      Right("1234") flatMap toInt shouldBe Right(1234)
    }

    "return function result when it's a Left and this is a Right" in {
      Right("xxx") flatMap toInt shouldBe Left("""For input string: "xxx"""")
    }

    "return this unchanged when it's a Left" in {
      Left("error") flatMap {x => Right("Prefix" + x)} shouldBe Left("error")
    }


  }

  "Either#orElse()" should {

    "return this when it's right"  in {
      Right(123) orElse Right(456) shouldBe Right(123)
    }

    "return argument when it's Right anf this is a Left" in {
      Left("error") orElse Right(456) shouldBe Right(456)
    }

    "return argument when it's Left anf this is a Right" in {
      Left("error") orElse Left("another error") shouldBe Left("another error")
    }

  }

  "Either#map2()" should {

    "return function result  wrapped in Right when this and an argument are both instances of Right" in {
      Right(1).map2(Right(2)) {_ + _} shouldBe Right(3)
    }

    "return this unchanged when it's a Left" in {
      Left("error").map2(Right("a")) {(x, y) => "Prefix" + x + y} shouldBe Left("error")
    }

    "return argument unchanged when it's a Left and the Either itself is a Right" in {
      Right(1).map2(Left("error")) {(x, y) => "Prefix" + x + y} shouldBe Left("error")
    }

  }

  "Either#traverse()" should {

    "combine result of applying given function to the list to Right of list of unwrapped results when all results are Rights" in {
      traverse(List(2, 4, 6)) {incrementEven} shouldBe Right(List(3, 5, 7))
    }

    "return Left with athe first error when result of applying function to the list element is Left for at least one element" in {
      traverse(List(2, 5, 6, 7)) {incrementEven} shouldBe Left("Argument should be an even number but was 5")
    }

    "return Right of empty list when argument is an empty list" in {
      traverse(List()) {x => Right(x.toString)} shouldBe Right(List())
    }

  }

  "Either#sequence()" should {

    "combine list of Rights to Right of list with the same unwrapped values" in {
      sequence(List(Right(1), Right(2), Right(3))) shouldBe Right(List(1, 2, 3))
    }

    "return Left when at least one argument in the original list is Left" in {
      sequence(List(Right(1), Left("error"), Right(2), Left("another error"))) shouldBe Left("error")
    }

    "return Right of empty list when argument is an empty list" in {
      sequence(List()) shouldBe Right(List())
    }

  }

  private def toInt(s: String): Either[String, Int] = try {
    Right(s.toInt)
  } catch {
    case e: NumberFormatException => Left(e.getMessage)
  }

  private def incrementEven(n: Int): Either[String, Int] = if (n % 2 == 0) {
    Right(n + 1)
  } else {
    Left(s"Argument should be an even number but was $n")
  }

}
