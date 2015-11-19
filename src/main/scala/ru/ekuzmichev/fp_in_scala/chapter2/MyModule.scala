package ru.ekuzmichev.fp_in_scala.chapter2

import scala.annotation.tailrec

object MyModule {
  def abs(n: Int): Int = {
    if (n < 0) -n
    else n
  }

  def factorial(n: Int): Int = {
    @tailrec def go(n: Int, acc: Int): Int = {
      if (n <= 0) acc
      else go(n - 1, n * acc)
    }
    go(n, 1)
  }

  private def formatResult(name: String, x: Int, f: Int => Int) = {
    val msg = "The %s of %d is %d"
    msg.format(name, x, f(x))
  }

  def fib(n: Int): Int = {
    @tailrec def go(n: Int, prev: Int = 0, next: Int = 1): Int = n match {
      case 0 => prev
      case 1 => next
      case _ => go(n - 1, next, next + prev)
    }
    go(n)
  }

  def fibNotTailRecursive(n: Int): Int = {
    if (n == 0) 0
    else if (n == 1) 1
    else fibNotTailRecursive(n - 1) + fibNotTailRecursive(n - 2)
  }

  def main(args: Array[String]): Unit =
    println(formatResult("absolute value", -42, abs))
    println(formatResult("factorial", 7, factorial))
}
