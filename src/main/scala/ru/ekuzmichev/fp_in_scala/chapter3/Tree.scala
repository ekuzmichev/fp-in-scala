package ru.ekuzmichev.fp_in_scala.chapter3

sealed trait Tree[+A]

case class Leaf[A](value: A) extends Tree[A]

case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  // Exercise 3.25
  def size[A](t: Tree[A]): Int =
    t match {
      case Leaf(_) => 1
      case Branch(left, right) => 1 + size(left) + size(right)
    }

  // Exercise 3.26
  def maximum(t: Tree[Int]): Int =
    t match {
      case Leaf(n) => n
      case Branch(left, right) => maximum(left) max maximum(right)
    }

  // Exercise 3.27
  def depth[A](t: Tree[A]): Int =
    t match {
      case Leaf(_) => 0
      case Branch(left, right) => 1 + (depth(left) max depth(right))
    }

  // Exercise 3.28
  def map[A, B](t: Tree[A])(f: A => B): Tree[B] =
    t match {
      case Leaf(n) => Leaf(f(n))
      case Branch(left, right) => Branch(map(left)(f), map(right)(f))
    }

  // Exercise 3.29
  def fold[A, B](t: Tree[A])(f: A => B)(g: (B, B) => B): B = t match {
    case Leaf(a) => f(a)
    case Branch(l, r) => g(fold(l)(f)(g), fold(r)(f)(g))
  }

  def sizeViaFold[A](t: Tree[A]): Int =
    fold(t)(_ => 1)(_ + _ + 1)

  def maximumViaFold(t: Tree[Int]): Int =
    fold(t)(a => a)(_ max _)

  def depthViaFold[A](t: Tree[A]): Int =
    fold(t)(_ => 0)((d1, d2) => 1 + (d1 max d2))
}
