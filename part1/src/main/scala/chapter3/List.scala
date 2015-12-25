package chapter3

import java.util.NoSuchElementException

import scala.annotation.tailrec

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

  def head[A](list: List[A]) : A = list match {
    case Nil => throw new NoSuchElementException("List is Nil, no head")
    case Cons(a, _) => a
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
      case Cons(a, t) if f(a) =>  dropWhile(t, f)
      case list => list
    }

  def init[A](l: List[A]): List[A] = {
    l match {
      case Nil => Nil
      case Cons(a, Nil) => Nil
      case Cons(a, t) => Cons(a, init(t))
    }
  }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B =
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x,y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _)

  def length[A](as: List[A]): Int = foldRight(as, 0)((list, acc) => acc +1)


  def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B): B = {
    @tailrec
    def loop(curr: List[A], acc: B): B = {
      curr match {
        case Nil => acc
        case Cons(x, xs) => loop(xs,f(acc, x))
      }
    }
    loop(as, z)
  }
}