package io.github.Ingwar.fpinscala.datastructures

import org.scalatest.{FlatSpec, Matchers}

class TreeSpec extends FlatSpec with Matchers {

  import Tree._

  "Tree#size()" should "return number of nodes (branches and leaves) in a tree" in {
    Tree.size(Branch(Leaf(1), Branch(Leaf(2), Leaf(3)))) shouldBe 5
  }

  it should "return 3 for Branch with leaves" in {
    Tree.size(Branch(Leaf(1), Leaf(2))) shouldBe 3
  }

  it should "return 1 for tree that has only one leaf node" in {
    Tree.size(Leaf(1)) shouldBe 1
  }

  "Tree#maximum()" should "return the biggest integer in a tree" in {
    maximum(Branch(Leaf(1), Branch(Leaf(4), Leaf(3)))) shouldBe 4
  }

  it should "return the data itself whe called on the leaf" in {
    maximum(Leaf(4)) shouldBe 4
  }

  it should "return biggest leaf element when called on the branch with only two leaves" in {
    maximum(Branch(Leaf(1), Leaf(2))) shouldBe 2
  }

  "Tree#depth()" should "should return maximum path length from a tree root to any of it's leaves" in {
    val tree =
      Branch(
        Branch(
          Branch(
            Leaf(1),
            Branch(
              Leaf(2),
              Leaf(3)
            )
          ),
          Leaf(5)
        ),
        Leaf(4)
      )

    depth(tree) shouldBe 4
  }

  it should "return 0 for tree with only one leaf node" in {
    depth(Leaf("a")) shouldBe 0
  }

  it should "return 1 for branch that contains only leaves nodes" in {
    depth(Branch(Leaf("a"), Leaf("b"))) shouldBe 1
  }

  "Tree#map()" should "construct new tree with the same structure as original one but with data replaced by result of application of provided function to the leaves data" in {
    map(Branch(Leaf(1), Branch(Leaf(2), Leaf(3)))) {_ + 2} shouldBe Branch(Leaf(3), Branch(Leaf(4), Leaf(5)))
  }

  it should "replace data in leaf node by result of provided function" in {
    map(Leaf("abc")) {_.length} shouldBe Leaf(3)
  }

  "Tree#fold()" should "allow as to reconstruct tree" in {
    val tree =
      Branch(
        Branch(
          Branch(
            Leaf(1),
            Branch(
              Leaf(2),
              Leaf(3)
            )
          ),
          Leaf(5)
        ),
        Leaf(4)
      )

    def leaf[A](value: A): Tree[A] = Leaf(value)

    fold(tree) {leaf} {Branch(_, _)} shouldBe tree
  }

  "Tree#sizeViaFold()" should "return number of nodes (branches and leaves) in a tree" in {
    sizeViaFold(Branch(Leaf(1), Branch(Leaf(2), Leaf(3)))) shouldBe 5
  }

  it should "return 3 for Branch with leaves" in {
    sizeViaFold(Branch(Leaf(1), Leaf(2))) shouldBe 3
  }

  it should "return 1 for tree that has only one leaf node" in {
    sizeViaFold(Leaf(1)) shouldBe 1
  }

  "Tree#maximumViaFold()" should "return the biggest integer in a tree" in {
    maximumViaFold(Branch(Leaf(1), Branch(Leaf(4), Leaf(3)))) shouldBe 4
  }

  it should "return the data itself whe called on the leaf" in {
    maximumViaFold(Leaf(4)) shouldBe 4
  }

  it should "return biggest leaf element when called on the branch with only two leaves" in {
    maximumViaFold(Branch(Leaf(1), Leaf(2))) shouldBe 2
  }

  "Tree#depthViaFold()" should "should return maximum path length from a tree root to any of it's leaves" in {
    val tree =
      Branch(
        Branch(
          Branch(
            Leaf(1),
            Branch(
              Leaf(2),
              Leaf(3)
            )
          ),
          Leaf(5)
        ),
        Leaf(4)
      )

    depthViaFold(tree) shouldBe 4
  }

  it should "return 0 for tree with only one leaf node" in {
    depthViaFold(Leaf("a")) shouldBe 0
  }

  it should "return 1 for branch that contains only leaves nodes" in {
    depthViaFold(Branch(Leaf("a"), Leaf("b"))) shouldBe 1
  }

  "Tree#mapViaFold()" should "construct new tree with the same structure as original one but with data replaced by result of application of provided function to the leaves data" in {
    mapViaFold(Branch(Leaf(1), Branch(Leaf(2), Leaf(3)))) {_ + 2} shouldBe Branch(Leaf(3), Branch(Leaf(4), Leaf(5)))
  }

  it should "replace data in leaf node by result of provided function" in {
    mapViaFold(Leaf("abc")) {_.length} shouldBe Leaf(3)
  }

}
