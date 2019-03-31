package io.github.Ingwar.fpinscala.laziness

import org.scalatest.{Matchers, OptionValues, WordSpec}

class StreamSpec extends WordSpec with Matchers with OptionValues {

  import Stream._

  "Stream" when {

    "empty" should {

      "return Empty from drop" in {
        Stream() drop 0 shouldBe Empty
      }

      "return Empty from takeWhile" in {
        Stream() takeWhile {_ => true} shouldBe Empty
      }

      "return Empty list from toList" in {
        Stream().toList shouldBe empty
      }

      "return Empty from take" in {
        Stream().take(1) shouldBe Empty
      }

      "return true from forAll" in {
       Stream() forAll {_ => true} shouldBe true
      }

      "return Empty from takeWhileViaFoldRight" in {
        Stream() takeWhileViAFoldRight  {_ => true} shouldBe Empty
      }

      "return None from headOption" in {
        Stream().headOption shouldBe None
      }

      "return Empty from map" in {
        Stream() map {_.toString} shouldBe Empty
      }

      "return Empty from filter" in {
        Stream() filter {_ => true} shouldBe Empty
      }

      "return argument from append" in {
        val a = Stream(1, 2, 3)
        Stream() append a shouldBe a
      }

      "return Empty from flatMap" in {
        Stream() flatMap {_ => Stream(1)} shouldBe Empty
      }

      "return Empty from mapViaUnfold" in {
        Stream() mapViaUnfold {_.toString} shouldBe Empty
      }

      "return Empty from takeViaUnfold" in {
        Stream().takeViAUnfold(1) shouldBe Empty
      }

      "return Empty from takeWhileViaUnfold" in {
        Stream() takeWhileViUnfold  {_ => true} shouldBe Empty
      }

      "return Empty from zipWith" in {
        Stream().zipWith(Stream(1, 2, 3)) {(a, b) => (a,b)} shouldBe Empty
      }

      "return all elements from argument paired with None" in {
        val zippedStream = Stream() zipAll Stream(1, 2, 3)
        zippedStream.toList shouldBe List(None -> Some(1), None -> Some(2), None -> Some(3))
      }

      "return false from startsWith" in {
        Stream() startsWith Stream(1, 2, 3) shouldBe false
      }

      "return stream with Empty as single element from tails" in {
        Stream().tails .toList shouldBe List(Empty)
      }

      "return Stream containing only initial element from scanRight" in {
        val scanResult = Stream.empty[Int].scanRight(0) {_ + _}
        scanResult.toList shouldBe List(0)
      }

    }

    "non-empty" should {

      "return Stream with first n elements dropped from drop(n)" in {
        Stream(1, 2, 3 ,4).drop(2).toList shouldBe List(3, 4)
      }

      "return Empty from drop(n) when n is bigger that the stream size" in {
        Stream(1, 2, 3) drop 4 shouldBe Empty
      }

      "return elements from takeWhile while predicate returns true for stream elements" in {
        Stream(2, 4, 5, 8).takeWhile(_ % 2 == 0).toList shouldBe List(2, 4)
      }

      "return whole stream from takeWhile is predicate is true for all elements" in {
        Stream(2, 4, 6).takeWhile(_ % 2 == 0).toList shouldBe List(2, 4, 6)
      }

      "return it's content from toList" in {
        Stream(1, 2, 3).toList shouldBe List(1, 2, 3)
      }

      "return stream with first n elements from take(n)" in {
        Stream(1, 2, 3, 4).take(2).toList shouldBe List(1, 2)
      }

      "return whole stream from take(n) when n is bigger than the stream size" in {
        Stream(1, 2).take(2).toList shouldBe List(1, 2)
      }

      "return true from forAll when all elements in stream match predicate" in {
        Stream(2, 4, 6 ,8) forAll {_ % 2 == 0} shouldBe true
      }

      "return false from forAll when at lest one element does not match predicate" in {
        Stream(2, 4, 5 ,8) forAll {_ % 2 == 0} shouldBe false
      }

      "return elements from takeWhileViaFoldRight while predicate returns true for stream elements" in {
        Stream(2, 4, 5, 8).takeWhileViAFoldRight(_ % 2 == 0).toList shouldBe List(2, 4)
      }

      "return whole stream from takeWhileViaFoldRight is predicate is true for all elements" in {
        Stream(2, 4, 6).takeWhileViAFoldRight(_ % 2 == 0).toList shouldBe List(2, 4, 6)
      }

      "return Some of head element from headOption" in {
        Stream(1, 2, 3).headOption shouldBe Some(1)
      }

      "return stream of transformed elements from map" in {
        val transformedStream = Stream(1, 2, 3) map {_.toString}
        transformedStream.toList shouldBe List("1", "2", "3")
      }

      "return stream with elements from this but with elements non-matching predicate removed" in {
        val filteredStream = Stream(1, 2, 3, 4, 5, 6) filter {_ % 2 == 0}
        filteredStream.toList shouldBe List(2, 4 ,6)
      }

      "return Empty from filter when none of the elements match predicate" in {
        Stream(1, 3, 5) filter {_ % 2 == 0} shouldBe Empty
      }

      "return this concatenated with argument from append" in {
        val concatenatedStreams = Stream(1, 2, 3) append Stream(4, 5, 6)
        concatenatedStreams.toList shouldBe List(1, 2, 3, 4, 5, 6)
      }

      "return concatenation of results of applying function to elements of stream from flatMap" in {
        val transformedStream = Stream(1, 2, 3) flatMap {x => Stream(x, x)}
        transformedStream.toList shouldBe List(1, 1, 2, 2, 3, 3)
      }

      "return Empty when function returns Empty for all elements of stream from flatMap" in {
        Stream(1, 2, 3) flatMap {_ => Empty} shouldBe Empty
      }

      "return stream of transformed elements from mapViaUnfold" in {
        val transformedStream = Stream(1, 2, 3) mapViaUnfold {_.toString}
        transformedStream.toList shouldBe List("1", "2", "3")
      }

      "return stream with first n elements from takeViaUnfold(n)" in {
        Stream(1, 2, 3, 4).takeViAUnfold(2).toList shouldBe List(1, 2)
      }

      "return whole stream from takeViaUnfold(n) when n is bigger than the stream size" in {
        Stream(1, 2).takeViAUnfold(2).toList shouldBe List(1, 2)
      }

      "return elements from takeWhileViaUnfold while predicate returns true for stream elements" in {
        Stream(2, 4, 5, 8).takeWhileViUnfold(_ % 2 == 0).toList shouldBe List(2, 4)
      }

      "return whole stream from takeWhileViaUnfold is predicate is true for all elements" in {
        Stream(2, 4, 6).takeWhileViUnfold(_ % 2 == 0).toList shouldBe List(2, 4, 6)
      }

      "return elements combined with argument's elements via provided function from zipWith" in {
        val zippedStream = Stream(1, 2, 3).zipWith(Stream(4, 5, 6)) {_ + _}
        zippedStream.toList shouldBe List(5, 7, 9)
      }

      "terminate zipWith results on the shortest stream" in {
        val zippedStream = Stream(1, 2).zipWith(Stream("a", "b", "c")) {(a, b) => a -> b}
        zippedStream.toList shouldBe List(1 -> "a", 2 -> "b")
      }

      "return its elements paired with arguments elements from zipAll" in {
        val zippedStream = Stream(1 ,2, 3) zipAll Stream("a", "b", "c")
        zippedStream.toList shouldBe List(Some(1) -> Some("a"), Some(2) -> Some("b"), Some(3) -> Some("c"))
      }

      "return None instead of it's elements when this is shorter then argument stream from zipAll" in {
        val zippedStream = Stream(1 ,2) zipAll Stream("a", "b", "c")
        zippedStream.toList shouldBe List(Some(1) -> Some("a"), Some(2) -> Some("b"), None -> Some("c"))
      }

      "return None instead of argument's stream elements when it's shorter then this stream from zipAll" in {
        val zippedStream = Stream(1 ,2, 3) zipAll Stream("a", "b")
        zippedStream.toList shouldBe List(Some(1) -> Some("a"), Some(2) -> Some("b"), Some(3) -> None)
      }

      "return true from startsWith when it's argument is a prefix for this stream" in {
        from(1) startsWith Stream(1, 2) shouldBe true
      }

      "return true from startsWith when it's argument is an empty Stream" in {
        constant("a") startsWith Stream() shouldBe true
      }

      "return stream of all possible suffixes (including the stream itself and empty stream) from tails" in {
        val tails = Stream(1, 2, 3).tails
        val materializedTails = tails.toList map {_.toList}
        materializedTails shouldBe List(List(1, 2, 3), List(2, 3), List(3), List())
      }

      "return stream of all intermediate results of applying provided function incrementally to it's elements starting from the end of the stream from scanRight" in {
        val prefixSums = Stream(1, 2, 3).scanRight(0) {_ + _}
        prefixSums.toList shouldBe List(6, 5, 3, 0)
      }

    }

  }

  "Stream#constant()" should {

    "return infinite stream consisting of given element" in {
      constant(3).take(5).toList shouldBe List(3, 3, 3, 3, 3)
      constant("x").take(2).toList shouldBe List("x", "x")
    }

  }

  "Stream#from()" should {

    "return infinite stream of consecutive integers starting from given number" in {
      val s = from(3)
      s.headOption.value shouldBe 3
      s.take(4).toList shouldBe List(3, 4, 5, 6)
    }

  }

  "Stream#fibs()" should {

    "return infinite stream of Fibonacci numbers" in {
      fibs.take(7).toList shouldBe List(0, 1, 1, 2, 3, 5, 8)
    }

  }

  "Stream#onesViaUnfold()" should {

    "return infinite stream of ones" in {
      onesViaUnfold.take(4).toList shouldBe List(1, 1, 1, 1)
    }

  }

  "Stream#constantViaUnfold()" should {

    "return infinite stream consisting of given element" in {
      constantViaUnfold(3).take(5).toList shouldBe List(3, 3, 3, 3, 3)
      constantViaUnfold("x").take(2).toList shouldBe List("x", "x")
    }

  }

  "Stream#fromViaUnfold()" should {

    "return infinite stream of consecutive integers starting from given number" in {
      val s = fromViaUnfold(3)
      s.headOption.value shouldBe 3
      s.take(4).toList shouldBe List(3, 4, 5, 6)
    }

  }

  "Stream#fibsViaUnfold()" should {

    "return infinite stream of Fibonacci numbers" in {
      fibsViaUnfold.take(7).toList shouldBe List(0, 1, 1, 2, 3, 5, 8)
    }

  }

}
