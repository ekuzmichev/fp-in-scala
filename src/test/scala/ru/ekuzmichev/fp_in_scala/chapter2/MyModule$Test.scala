package ru.ekuzmichev.fp_in_scala.chapter2

import org.scalatest._
import ru.ekuzmichev.fp_in_scala.chapter2.MyModule.fib

class MyModule$Test extends FunSuite with Matchers {

  test("testFib") {
    fib(0) should be (0)
    fib(1) should be (1)
    fib(5) should be (5)
  }

}
