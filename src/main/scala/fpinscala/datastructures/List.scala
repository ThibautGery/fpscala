package fpinscala.datastructures

import scala.annotation.tailrec


sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def tail[A](list: List[A]): List[A] = list match {
    case Nil => throw new NoSuchElementException("The list is empty")
    case Cons(x, xs) => xs
  }

  def sum(list: List[Int]): Int = list match {
    case Cons(x, xs) => x + sum(xs)
    case _ => 0
  }

  def product(ds: List[Double]): Double = {
    ds match {
      case Nil => 1
      case Cons(0, _) => 0
      case Cons(x, xs) => x * product(xs)
    }
  }

  def apply[A](items: A*): List[A] = {
    if(items.isEmpty) Nil
    else Cons(items.head, apply(items.tail: _*))
  }

  def setHead[A](newHead: A, list: List[A]): List[A] = Cons(newHead, List.tail(list))

  @tailrec
  def drop[A](l: List[A], n: Int): List[A] = {
    if(n == 0) l
    else drop(tail(l), n - 1)
  }

}