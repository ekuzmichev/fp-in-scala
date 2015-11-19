// Exercise 2.1
def findFirst[A](as: Array[A], p: A => Boolean): Int = {
  @annotation.tailrec
  def loop(n: Int): Int = {
    if (n >= as.length) -1
    else if (p(as(n))) n
    else loop(n + 1)
  }

  loop(0)
}

findFirst(Array("1", "2", "3"), (s: String) => s == "4")

// Exercise 2.2
def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
  @annotation.tailrec
  def loop(ass: Array[A], acc: Boolean): Boolean = {
    if (ass.length <= 1) true
    else ordered(ass.head, ass.tail.head) && loop(ass.tail, acc)
  }
  loop(as, acc = true)
}
@annotation.tailrec
def isSorted2[A](as: List[A], ordered: (A, A) => Boolean): Boolean = as match {
  case List() => true
  case List(_) => true
  case head :: tail => ordered(head, tail.head) && isSorted2(tail, ordered)

}
isSorted(Array(3, 2, 4), (a: Int, b: Int) => a <= b)
isSorted2(List(3, 2, 4), (a: Int, b: Int) => a <= b)

val sum: (Int, Int) => Int = (a, b) => a + b

// Exercise 2.3
def curry[A, B, C](f: (A, B) => C): A => B => C = {
  a => b => f(a, b)
}
sum(1, 2)
curry(sum)(1)(2)

// Exercise 2.4
def uncurry[A, B, C](f: A => B => C): (A, B) => C = {
  (a, b) => f(a)(b)
}

sum(1, 2)
uncurry(curry(sum))(1, 2)

// Exercise 2.5
def compose[A, B, C](f: B => C, g: A => B): A => C = {
  a => f(g(a))
}

def format = (a: Int) => "This is %d".format(a)
def double = (a: Int) => 2 * a

compose(format, double)(1)
