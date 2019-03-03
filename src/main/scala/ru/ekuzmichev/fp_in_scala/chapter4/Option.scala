package ru.ekuzmichev.fp_in_scala.chapter4

sealed trait Option[+A] {
  def isEmpty: Boolean

  def get: A

  def map[B](f: A => B): Option[B] =
    this match {
      case None => None
      case Some(x) => Some(f(x))
    }

  def flatMap[B](f: A => Option[B]): Option[B] = ???

  // 'B >: A' means that parameter B must be a equal to or supertype of A
  // ': => B' means that default is of type B, but won't be evaluated until it will be needed
  def getOrElse[B >: A](default: => B): B =
    this match {
      case None => default
      case Some(x) => x
    }

  def orElse[B >: A](ob: => Option[B]): Option[B] = ???


  def filter(f: A => Boolean): Option[A] = ???
}

case class Some[+A](get: A) extends Option[A] {
  override def isEmpty: Boolean = false
}

case object None extends Option[Nothing] {
  override def isEmpty: Boolean = true

  override def get: Nothing = throw new Exception("None.get")
}
