package ru.ekuzmichev.fp_in_scala.chapter3

import org.scalatest.{FunSuite, Matchers}

class TreeTest extends FunSuite with Matchers {
  test("testSize") {
    Tree.size(Leaf(1)) should be(1)
    Tree.size(Branch(Leaf(1), Leaf(2))) should be(3)
    Tree.size(Branch(Leaf(1), Branch(Leaf(2), Leaf(3)))) should be(5)
  }

  test("testMaximum") {
    Tree.maximum(Leaf(1)) should be(1)
    Tree.maximum(Branch(Leaf(1), Leaf(2))) should be(2)
    Tree.maximum(Branch(Leaf(1), Branch(Leaf(2), Leaf(3)))) should be(3)
  }

  test("testDepth") {
    Tree.depth(Leaf(1)) should be(0)
    Tree.depth(Branch(Leaf(1), Leaf(2))) should be(1)
    Tree.depth(Branch(Leaf(1), Branch(Leaf(2), Leaf(3)))) should be(2)
    Tree.depth(Branch(Branch(Leaf(1), Branch(Leaf(2), Leaf(3))), Branch(Leaf(4), Leaf(5)))) should be(3)
  }

  test("testMap") {
    Tree.map(Leaf(1))(_ * 2) should be(Leaf(2))
    Tree.map(Branch(Leaf(1), Leaf(2)))(_.toString) should be(Branch(Leaf("1"), Leaf("2")))
  }

  test("testSizeVIaFold") {
    Tree.sizeViaFold(Leaf(1)) should be(1)
    Tree.sizeViaFold(Branch(Leaf(1), Leaf(2))) should be(3)
    Tree.sizeViaFold(Branch(Leaf(1), Branch(Leaf(2), Leaf(3)))) should be(5)
  }

  test("testMaximumViaFold") {
    Tree.maximumViaFold(Leaf(1)) should be(1)
    Tree.maximumViaFold(Branch(Leaf(1), Leaf(2))) should be(2)
    Tree.maximumViaFold(Branch(Leaf(1), Branch(Leaf(2), Leaf(3)))) should be(3)
  }

  test("testDepthViaFold") {
    Tree.depthViaFold(Leaf(1)) should be(0)
    Tree.depthViaFold(Branch(Leaf(1), Leaf(2))) should be(1)
    Tree.depthViaFold(Branch(Leaf(1), Branch(Leaf(2), Leaf(3)))) should be(2)
    Tree.depthViaFold(Branch(Branch(Leaf(1), Branch(Leaf(2), Leaf(3))), Branch(Leaf(4), Leaf(5)))) should be(3)
  }
}
