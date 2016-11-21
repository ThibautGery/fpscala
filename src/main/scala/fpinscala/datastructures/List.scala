package fpinscala.datastructures


sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(list: List[Int]): Int = list match {
    case Cons(x, xs) => x + sum(xs)
    case _ => 0
  }

  def apply[A](items: A*): List[A] = {
    if(items.isEmpty) Nil
    else Cons(items.head, apply(items.tail: _*))
  }
}