package chapter3

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }


  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def tail[A](list: List[A]): List[A] = list match {
    case Nil => throw new NoSuchElementException("List is Nil not tail")
    case Cons(_, t) => t
  }

  def setHead[A](newHead: A, list: List[A]): List[A] = newHead match {
    case Nil => Nil
    case a => Cons(a, tail(list))
  }

  def drop[A](l: List[A], n: Int): List[A] =
    if( n == 0) {
      l
    } else {
      drop(tail(l), n-1)
    }


  def dropWhile[A](l: List[A], f: A => Boolean): List[A] =
    l match {
      case Nil => Nil
      case Cons(a, t) => if(f(a)) dropWhile(t, f) else l
    }

}