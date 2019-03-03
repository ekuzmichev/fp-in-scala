package ru.ekuzmichev.fp_in_scala.chapter7

object Chapter7 extends App {
  def sum(ints: IndexedSeq[Int]): Int = {
    if (ints.size <= 1)
      ints.headOption.getOrElse(0)
    else {
      val (l, r) = ints.splitAt(ints.size / 2)
      sum(l) + sum(r)
    }
  }

  val res = sum(Vector(1, 2, 3, 4, 5))

  println(s"Result: $res")
}
