package io.github.Ingwar.fpinscala.errorhandling

import org.scalatest.{Matchers, WordSpec}

class OptionSpec extends WordSpec with Matchers {

  import Option._

  "An Option#map()" should {

    "return transformed value wrapped in Some when Option is defined " in {
      Some(1) map {_.toString} shouldBe Some("1")
    }

    "return None when Option is not defined" in {
      None map {x => x} shouldBe None
    }

  }

  "An Option#getOrElse()" should {

    "return Option's value when it's defined " in {
      Some(1) getOrElse Some(3) shouldBe 1
    }

    "return None when Option is not defined" in {

    }

  }

  "An Option#flatMap()" should {

    "return transformed value wrapped in Some when Option is defined and function returns Some" in {
      Some(2) flatMap {x => Some(x + 1)} shouldBe Some(3)
    }

    "return None when Option  is defined and function returns None" in {
      Some("aa") flatMap {_ => None} shouldBe None
    }

    "return None when Option is not defined" in {
      None flatMap {Some(_)} shouldBe None
    }

  }

  "An Option#orElse()" should {

    "return Option itself when it's defined " in {
      Some(1) orElse None shouldBe Some(1)
    }

    "return other operand when Option is not defined" in {
      None orElse Some(1) shouldBe Some(1)
    }

    "return None when both Options are not defined" in {
      None orElse None shouldBe None
    }

  }

  "An Option#filter()" should {

    "return it's value unchanged when Option is defined and predicate returns true" in {
      Some(2) filter { _ % 2 == 0} shouldBe Some(2)
    }

    "return None when Option is defined and predicate returns false" in {
      Some(3) filter { _ % 2 == 0} shouldBe None
    }

    "return None when Option is not defined" in {
      None filter {_ => true} shouldBe None
    }

  }

  "An Option#variance()" should {

    "compute the sequence variance when called on non-empty sequence" in {
      variance(Seq(1, 2, 3, 4, 5)) shouldBe Some(2.0)
    }

    "return None when called on empty sequence" in {
      variance(Seq.empty) shouldBe None
    }

  }

  "An Option#map2()" should {

    "apply given function to arguments wrapped in Options and return result, also wrapped in Option" in {
      map2(Some(1), Some(2)) {_ + _} shouldBe Some(3)
    }

    "return None when first argument is None" in {
      map2(None: Option[Int], Some(2)) {_ + _} shouldBe None
    }

    "return None when second argument is None" in {
      map2(Some(1), None: Option[Int]) {_ + _} shouldBe None
    }

  }

  "An Option#sequence()" should {

    "combine list of Some's to Some of list with the same unwrapped values" in {
      sequence(List(Some(1), Some(2), Some(3))) shouldBe Some(List(1, 2, 3))
    }


    "return None when list contains at least one None" in {
      sequence(List(Some(1), None, Some(2))) shouldBe None
    }

    "return Some(List()) when argument is an empty list" in {
      sequence(List()) shouldBe Some(List())
    }

  }

  "An Option#traverse()" should {

    "combine results of applying given function to the elements of the list to optional list" in {
      traverse(List("1", "2", "3")) {x => Some(x.toInt)} shouldBe Some(List(1, 2, 3))
    }

    "return None when function returns none at least for one element" in {
      traverse(List(2, 4, 6, 7, 8)) {x => if (x % 2 == 0) Some(x) else None} shouldBe None
    }

    "return Some of empty list when called on empty list" in {
      traverse(List()) {_ => None} shouldBe Some(List())
    }

  }


  "An Option#sequenceViaTraverse()" should {

    "combine list of Some's to Some of list with the same unwrapped values" in {
      sequenceViaTraverse(List(Some(1), Some(2), Some(3))) shouldBe Some(List(1, 2, 3))
    }


    "return None when list contains at least one None" in {
      sequenceViaTraverse(List(Some(1), None, Some(2))) shouldBe None
    }

    "return Some(List()) when argument is an empty list" in {
      sequenceViaTraverse(List()) shouldBe Some(List())
    }

  }

}
