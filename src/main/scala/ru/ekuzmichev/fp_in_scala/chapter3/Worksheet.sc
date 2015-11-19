import scala.annotation.tailrec
// Exercise 3.1
List(1, 2, 3, 4, 5) match {
  case x :: 2 :: 4 :: _ => x
  case Nil => 42
  case x :: y :: 3 :: 4 :: _ => x + y
  case h :: t => h + t.sum
  case _ => 101
}
// Exercise 3.2
def tail[A](l: List[A]): List[A] = l match {
  case Nil => Nil
  case _ :: xs => xs
}
tail(List())
tail(List(1, 2, 3, 4, 5))
// Exercise 3.3
def setHead[A](l: List[A], h: A): List[A] = l match {
  case Nil => Nil
  case _ :: xs => h :: xs
}
setHead(List(1, 2, 3), 3)
setHead(List(), 3)
// Exercise 3.4
def drop[A](l: List[A], n: Int): List[A] = {
  if (n <= 0) l
  else l match {
    case Nil => Nil
    case _ :: xs => drop(xs, n - 1)
  }
}

drop(List(1, 2, 3, 4, 5), 3)
// Exercise 3.5
def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
  case x :: xs if f(x) => dropWhile(xs, f)
  case _ => l
}
dropWhile(List(1, 2, 3, 4, 5), (x: Int) => x < 4)
def dropWhile2[A](l: List[A])(f: A => Boolean): List[A] = l match {
  case x :: xs if f(x) => dropWhile2(xs)(f)
  case _ => l
}
dropWhile2(List(1, 2, 3, 4, 5))(x => x < 4) // NOTE: type inference
def append[A](a1: List[A], a2: List[A]): List[A] = a1 match {
    case Nil => a2
    case x :: xs => x :: append(xs, a2)
  }
append(List(), List())
append(List(), List(1, 2, 3))
append(List(4, 5, 6), List(1, 2, 3))
// Exercise 3.6
def init[A](l: List[A]): List[A] = l match {
  case Nil => Nil
  case x :: Nil => Nil
  case x :: xs => x :: init(xs)
}
init(List(1, 2, 3, 4, 5))
init(List(1, 2))
init(List(1))
init(List())
def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B =
  as match {
    case Nil => z
    case x :: xs => f(x, foldRight(xs, z)(f))
  }
foldRight(List(1, 2, 3, 4, 5), 0)(_ + _)
foldRight(List(1, 2, 3, 4, 5), 1)(_ * _)
//foldRight((1 to 1000000).toList, 1)(_ * _) // StackOverflowError
// Exercise 3.7
def foldRight2[A, B](l: List[A], z: B)(f: (A, B) => B): B =
  l.foldLeft(z)((b, a) => f(a, b))
foldRight2((1 to 1000000).toList, 1)(_ + _)

def getTestList(n: Int) = (1 to n).toList

def foldRight3[A, B](l: List[A], z: B)(f: (A, B) => B): B =
  l match {
    case Nil => z
    case x :: xs => f(x, foldRight3(xs, z)(f))
  }
foldRight3(List(1, 2, 3), 0.0)(_ + _)
// Exercise 3.7
def foldRight4[A, B](l: List[A], z: B)(f: (A, B) => B) =
  l.reverse.foldLeft(z)((a, b) => f(b, a))
foldRight4(getTestList(10000), 1.0)(_ * _)
foldRight4(getTestList(10000), 0.0)(_ + _)
// Exercise 3.8
foldRight(List(1, 2, 3), Nil: List[Int])(_ :: _)
// Exercise 3.9
def length[A](l: List[A]) =
  foldRight(l, 0)((_, acc) => acc + 1)
length(List())
length(List(1, 2, 3))

// Exercise 3.10
@tailrec def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B =
  l match {
    case Nil => z
    case x :: xs => foldLeft(xs, f(z, x))(f)
  }

foldLeft(List(1, 2, 3), 0.0)(_ + _)
foldLeft(getTestList(100000), 0.0)(_ + _) // NOTE: No StackOverflowError

// Exercise 3.11
def sum(l: List[Int]): Int =
  foldLeft(l, 0)(_ + _)
sum(List(1, 2, 3))
def product(l: List[Int]): Double =
  foldLeft(l, 1)(_ * _)
product(List(1, 2, 3))
// Exercise 3.12
def reverse[A](l: List[A]): List[A] =
  l match {
    case x :: xs => reverse(xs) :+ x
    case _ => l
  }

reverse(List(1, 2, 3))
def reverse2[A](l: List[A]): List[A] =
  foldLeft(l, Nil: List[A])((b, a) => a :: b)

reverse2(List())
reverse2(List(1, 2, 3))
// Exercise 3.13
// for foldRight implemented via foldLeft see ex.3.7 foldRight2
def foldLeft2[A, B](l: List[A], z: B)(f: (B, A) => B): B =
  foldRight(l.reverse, z)((a, b) => f(b, a))
foldLeft2(List(1, 2, 3), 1.0)(_ * _)
// foldLeft2(getTestList(1000000), 1.0)(_ * _) // NOTE: StackOverFlowError due to implementation via foldRight

// Exercise 3.14
def append2[A](l: List[A], r: List[A]): List[A] =
  foldRight(l, r)(_ :: _)

append2(List(1, 2, 3), List(4))
// Exercise 3.15
def concat[A](l: List[List[A]]): List[A] =
  foldLeft(l, Nil: List[A])(append2)
concat(List(List(1, 2, 3), List(4), List(5)))

// Exercise 3.16
def plusOne(l: List[Int]) =
  foldRight(l, Nil: List[Int])(_ + 1 :: _)

plusOne(List(1, 2, 3))
// Exercise 3.17
def toString(l: List[Double]) =
  foldRight(l, Nil: List[String])(_.toString :: _)
toString(List(1, 2, 3))

// Exercise 3.18
def map[A, B](as: List[A])(f: A => B): List[B] =
  foldRight(as, Nil: List[B])(f(_) :: _)

map(List(1, 2, 3))(_ * 2)
map(List(1.0, 2.0, 3.0))(_.toString)

// Exercise 3.19
def filter[A](as: List[A])(f: A => Boolean) =
  foldRight(as, Nil: List[A])((a, b) =>
    if (f(a)) a :: b else b
  )

filter(List(1, 2, 3, 4, 5))(_ % 2 == 0)
// Exercise 3.20
def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] =
  concat(map(as)(f))

flatMap(List(1, 2, 3))(_ :: List(5, 5))
// Exercise 3.21
def filter2[A](as: List[A])(f: A => Boolean) =
  flatMap(as)(x => if (f(x)) List(x) else Nil)

filter2(List(1, 2, 3, 4, 5))(_ % 2 == 0)
// Exercise 3.22
def sum(l: List[Int], r: List[Int]): List[Int] = (l, r) match {
  case (_, Nil) => Nil
  case (Nil, _) => Nil
  case (x :: xs, y :: ys) => (x + y) :: sum(xs, ys)
}

sum(List(1, 2, 3), List(4, 5, 6))
// Exercise 3.23
def zipWith[A, B, C](l: List[A], r: List[B])(f: (A, B) => C): List[C] = (l, r) match {
  case (_, Nil) => Nil
  case (Nil, _) => Nil
  case (x :: xs, y :: ys) => f(x, y) :: zipWith(xs, ys)(f)
}

zipWith(List(1, 2, 3), List(4, 5, 6))(_ + _)
zipWith(List(1, 2, 3), List("4", "5", "6"))(_ + _.toInt)

List(1, 2, 3).scanLeft(0)(_ + _)
getTestList(10).scanLeft(0)(_ + _)
getTestList(10).foldLeft(0)(_ + _)
getTestList(10).scanRight(0)(_ + _)
getTestList(10).foldRight(0)(_ + _)
// Exercise 3.24
@tailrec def startsWith[A](l: List[A], prefix: List[A]): Boolean = (l, prefix) match {
  case (_, Nil) => true
  case (x :: xs, y :: ys) if x == y => startsWith(xs, ys)
  case _ => false
}
@tailrec def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = sup match {
  case Nil => sub == Nil
  case _ if startsWith(sup, sub) => true
  case x :: xs => hasSubsequence(xs, sub)
}
hasSubsequence(List(1, 2, 3), List())
hasSubsequence(List(1, 2, 3), List(1))
hasSubsequence(List(1, 2, 3), List(1, 2))
hasSubsequence(List(1, 2, 3), List(2))
hasSubsequence(List(1, 2, 3), List(2, 3))
hasSubsequence(List(1, 2, 3), List(1, 2, 3))
hasSubsequence(List(1, 2, 3), List(1, 3))
hasSubsequence(List(1, 2, 3), List(1, 1))