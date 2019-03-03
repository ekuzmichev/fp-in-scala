package ru.ekuzmichev.fp_in_scala.chapter7

object Chapter7 extends App {
  def sum(ints: IndexedSeq[Int]): Par[Int] = {
    if (ints.size <= 1)
      Par.unit(ints.headOption.getOrElse(0))
    else {
      val (l, r) = ints.splitAt(ints.size / 2)
      Par.map2(Par.fork(sum(l)), Par.fork(sum(r)))(_ + _)
    }
  }
}

trait Par[A]

object Par {
  def unit[A](a: A): Par[A] = ???

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  def run[A](a: Par[A]): A = ???

  def map2[A, B, C](parA: Par[A], parB: Par[B])(f: (A, B) => C): Par[C] = ???

  def fork[A](a: => Par[A]): Par[A] = ???
}
