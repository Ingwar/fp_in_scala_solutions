package io.github.Ingwar.fpinscala.datastructures

import org.scalatest.{FlatSpec, Matchers}

class ListSpec extends FlatSpec with Matchers {

  import List._

  "List#tail()" should "return Nil when called on empty list" in {
    tail(List()) shouldBe Nil
  }

  it should "return Nil when called on one-element list" in {
    tail(List(1)) shouldBe Nil
  }

  it should "return Cons with the rest of list otherwise" in {
    tail(List(1, 2, 3, 4)) shouldBe List(2, 3, 4)
  }

  "List#setHead()" should "throw an error when called on empty list" in {
    the[Exception] thrownBy {
      setHead(List(), 1)
    } should have message "Attempt to set a head of an empty list"
  }

  it should "replace head otherwise" in {
    setHead(List(1), 2) shouldBe List(2)
    setHead(List(1, 2), 2) shouldBe List(2, 2)
  }

  "List#drop()" should "drop n first elements of the list" in {
    drop(List(1, 2, 3, 4, 5), 2) shouldBe List(3, 4, 5)
  }

  it should "return Nil when called on the empty list" in {
    drop(List(), 2) shouldBe List()
  }

  it should "return Nil when n is bigger than the list size" in {
    drop(List(1, 2, 3), 4) shouldBe List()
  }

  it should "return the same list when n is zero" in {
    drop(List(1), 0) shouldBe List(1)
    drop(List(), 0) shouldBe List()
  }

  it should "thrown an error when n is negative" in {
    the[IllegalArgumentException] thrownBy {
      drop(List(1, 2, 3), -1)
    } should have message "requirement failed: n should be non-negative but was -1"
  }

  "List#dropWhile()" should "drop first valid elements from the list" in {
    dropWhile(List(2, 3, 4), isEven) shouldBe List(3, 4)
  }

  it should "return Nil when called on the empty list" in {
    dropWhile(List(), isEven)
  }


  it should "return Nil when all elements in the list are valid" in {
    dropWhile(List(2, 4, 6, 8), isEven) shouldBe List()
  }

  it should "return the same list when the first element in the list is not valid" in {
    dropWhile(List(3, 4, 5, 6), isEven) shouldBe List(3, 4, 5, 6)
  }

  "List#init()" should "return all list elements except the las one" in {
    init(List(1, 2, 3, 4)) shouldBe List(1, 2, 3)
  }

  it should "return Nil when called on empty list" in {
    init(List()) shouldBe List()
  }

  it should "return Nil when called on single-element list" in {
    init(List(1)) shouldBe List()
  }

  "List#length()" should  "return the length of the list" in {
    List.length(List(1, 2, 3, 4)) shouldBe 4
  }

  it should "return zero when called on the empty list" in {
    List.length(List()) shouldBe 0
  }

  "List#foldLeft()" should "compute the same sum value as foldRight" in {
    foldLeft(List(1, 2, 3, 4), 0) {_ + _} shouldEqual foldRight(List(1, 2, 3, 4), 0) {_ + _}
  }

  it should "compute the same product value as foldRight" in {
    foldLeft(List(1, 2, 3, 4), 1) {_ * _} shouldEqual foldRight(List(1, 2, 3, 4), 1) {_ * _}
  }

  it should "compute the same concatenation value as foldRight" in {
    foldLeft(List("a", "b", "c"), "") {_ + _} shouldEqual foldRight(List("a", "b", "c"), "") {_ + _}
  }

  "List#sum()" should "return sum of list elements" in {
    sum(List(1, 2, 3, 4)) shouldBe 10
  }

  it should "return element itself when called on single-element list" in {
    sum(List(9)) shouldBe 9
  }

  it should "return zero when called on empty list" in {
    sum(List()) shouldBe 0
  }

  "List#product()" should "return product of list elements" in {
    product(List(1, 2, 3, 4)) shouldBe 24
  }

  it should "zero when list contains at least one zero" in {
    product(List(1, 2, 3, 0)) shouldBe 0
  }

  it should "element itself when called on single-elemnt list" in {
    product(List(3)) shouldBe 3
  }

  it should "return one whe called on empty list" in {
    product(List()) shouldBe 1
  }

  "List#length2()" should  "return the length of the list" in {
    length2(List(1, 2, 3, 4)) shouldBe 4
  }

  it should "return zero when called on the empty list" in {
    length2(List()) shouldBe 0
  }

  "List#reverese()" should "reverse order of elements in the list" in {
    reverse(List(1, 2, 3, 4)) shouldBe List(4, 3, 2, 1)
  }

  it should "return single-element list unchanged" in {
    reverse(List(2)) shouldBe List(2)
  }

  it should "return Nil when called on empty list" in {
    reverse(List()) shouldBe List()
  }

  "List#foldLeftViaFoldRight()" should "compute the same sum value as foldLeft" in {
    foldLeftViaFoldRight(List(1, 2, 3, 4), 0) {_ + _} shouldEqual foldLeft(List(1, 2, 3, 4), 0) {_ + _}
  }

  it should "compute the same product value as foldLeft" in {
    foldLeftViaFoldRight(List(1, 2, 3, 4), 1) {_ * _} shouldEqual foldLeft(List(1, 2, 3, 4), 1) {_ * _}
  }

  it should "compute the same concatenation value as foldLeft" in {
    foldLeftViaFoldRight(List("a", "b", "c"), "") {_ + _} shouldEqual foldLeft(List("a", "b", "c"), "") {_ + _}
  }

  "List#foldRightViaFoldLeft()" should "compute the same sum value as foldRight" in {
    foldRightViaFoldLeft(List(1, 2, 3, 4), 0) {_ + _} shouldEqual foldRight(List(1, 2, 3, 4), 0) {_ + _}
  }

  it should "compute the same product value as foldRight" in {
    foldRightViaFoldLeft(List(1, 2, 3, 4), 1) {_ * _} shouldEqual foldRight(List(1, 2, 3, 4), 1) {_ * _}
  }

  it should "compute the same concatenation value as foldRight" in {
    foldRightViaFoldLeft(List("a", "b", "c"), "") {_ + _} shouldEqual foldRight(List("a", "b", "c"), "") {_ + _}
  }

  "List#append()" should "concatenate lists passed as arguments" in {
    append(List(1, 2), List(3, 4)) shouldBe List(1, 2, 3, 4)
    append(List(1, 2, 3), List(4)) shouldBe List(1, 2, 3, 4)
    append(List(1), List(2, 3, 4)) shouldBe List(1, 2, 3, 4)
  }

  it should "return other argument he one of the lists is Nil" in {
    append(List(1, 2), List()) shouldBe List(1, 2)
    append(List(), List(1, 2)) shouldBe List(1, 2)
  }

  it should "return Nil when both arguments are empty" in {
    append(List(), List()) shouldBe List()
  }

  "List#concat()" should "concatenate list passed as arguments in one flat list" in {
    concat(List(List(1), List(2), List(3), List(4))) shouldBe List(1, 2, 3, 4)
    concat(List(List(1, 2), List(3), List(4))) shouldBe List(1, 2, 3, 4)
    concat(List(List(1, 2), List(3, 4))) shouldBe List(1, 2, 3, 4)
  }

  it should "return empty list when called on empty list" in {
    concat(List()) shouldBe List()
  }

  it should "return non-empty argument when all other arguments are empty lists" in {
    concat(List(List(1, 2), List(), List())) shouldBe List(1, 2)
    concat(List(List(), List(1, 2), List())) shouldBe List(1, 2)
    concat(List(List(), List(), List(1, 2))) shouldBe List(1, 2)
  }

  "List#addOne()" should "add one to each integer element in the list" in {
    addOne(List(1, 2, 3, 4)) shouldBe List(2, 3, 4, 5)
  }

  it should "return empty list when called on empty list" in {
    addOne(List()) shouldBe List()
  }

  "List#doublesToStrings()" should "convert each double in the list to corresponding string" in {
    doublesToStrings(List(1, 2, 3)) shouldBe List("1.0", "2.0", "3.0")
  }

  it should "return empty list when called on empty list" in {
    doublesToStrings(List()) shouldBe List()
  }

  "List#map()" should "return new list where each element is a result of applying provided function to the corresponding element of the given list" in {
    map(List(1, 2, 3, 4)) {_ + 2} shouldBe List(3, 4, 5, 6)
  }

  it should "return empty list when called on empty list" in {
    map(List())(x => x) shouldBe List()
  }

  "List#filter()" should "return new list that contains only elements that satisfy predicate" in {
    filter(List(1, 2, 3, 4, 5, 6, 7, 8)) {isEven} shouldBe List(2, 4, 6, 8)
  }

  it should "return empty list when called on empty list" in {
    filter(List()) {isEven} shouldBe List()
  }

  it should "return original list when all it's elements satisfy predicate" in {
      filter(List(1, 3, 5, 7)) {isOdd} shouldBe List(1, 3, 5, 7)
    }

  it should "return empty list when none of original list's elements satisfy predicate" in {
    filter(List(2, 4, 6, 8)) {isOdd} shouldBe List()
  }

  "List#flatMap()" should "return concatenation of results of applying function to elements of original list" in {
    flatMap(List(1, 2, 3, 4)) {i => List(i, i)} shouldBe List(1, 1, 2, 2, 3, 3, 4, 4)
  }

  it should "return empty list when called on empty list" in {
    flatMap(List()) {x => x} shouldBe List()
  }

  it should "return empty list when result of each function call is empty list" in {
    flatMap(List(1, 2, 3, 4)) {_ => List()} shouldBe List()
  }

  "List#filterViaFlatMap()" should "return new list that contains only elements that satisfy predicate" in {
    filterViaFlatMap(List(1, 2, 3, 4, 5, 6, 7, 8)) {isEven} shouldBe List(2, 4, 6, 8)
  }

  it should "return empty list when called on empty list" in {
    filterViaFlatMap(List()) {isEven} shouldBe List()
  }

  it should "return original list when all it's elements satisfy predicate" in {
      filterViaFlatMap(List(1, 3, 5, 7)) {isOdd} shouldBe List(1, 3, 5, 7)
    }

  it should "return empty list when none of original list's elements satisfy predicate" in {
    filterViaFlatMap(List(2, 4, 6, 8)) {isOdd} shouldBe List()
  }

  "List#addLists()" should "add two integer lists elementwise" in {
    addLists(List(1, 2, 3), List(4, 5, 6)) shouldBe List(5, 7, 9)
  }

  it should "return empty lists when both arguments are empty lists" in {
    addLists(List(), List()) shouldBe List()
  }

  it should "truncate output to the length of shortest argument" in {
    addLists(List(1, 2, 3), List(4, 5, 6, 7)) shouldBe List(5, 7, 9)
  }

  "List#zipWith()" should "combine argument lists by calling provided function on pairs of corresponding elements" in {
    zipWith(List(1, 2, 3), List("a", "b", "c")) {_ -> _} shouldBe List((1, "a"), (2, "b"), (3, "c"))
  }

  it should "return empty lists when arguments are empty lists" in {
    zipWith(List(), List())((_, _)) shouldBe List()
  }

  it should "truncate output to the length of shortest argument" in {
    zipWith(List(1, 2, 3), List(4, 5, 6, 7)){_ + _} shouldBe List(5, 7, 9)
  }

  "List#hasSubsequence()" should "return true when first argument contains second one" in {
    hasSubSequence(List(1, 2, 3, 4), List(2)) shouldBe true
    hasSubSequence(List(1, 2, 3, 4), List(2, 3)) shouldBe true
    hasSubSequence(List(1, 2, 3, 4), List(1, 2)) shouldBe true
  }

  it should "return true when arguments are equal" in {
    hasSubSequence(List(1, 2, 3), List(1, 2 ,3)) shouldBe true
  }

  it should "return false when first argument does not contain second one" in {
    hasSubSequence(List(1, 2, 3, 4), List(6, 7)) shouldBe false
    hasSubSequence(List(1, 2, 3, 4), List(6)) shouldBe false
  }

  it should "return false when first argument is an empty list" in {
    hasSubSequence(List(), List(2, 3)) shouldBe false
  }

  it should "return true when second argument is an empty list" in {
    hasSubSequence(List(1, 2, 3, 4), List()) shouldBe true
  }

  private def isEven (a: Int) = a % 2 == 0

  private def isOdd (a: Int) = a % 2 == 1

}
