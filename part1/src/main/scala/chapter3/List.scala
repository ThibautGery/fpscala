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

  def sum3(ns: List[Int]) =
    foldLeft(ns, 0)((x,y) => x + y)

  def product3(ns: List[Double]) =
    foldLeft(ns, 1.0)(_ * _)

  def reverse[A](l: List[A]): List[A] = foldLeft(l, Nil: List[A])((acc, elem) => Cons(elem, acc))


  def foldRight2[A,B](as: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(reverse(as), z)((a, b) => f(b, a))


  def append[A](a1: List[A], a2: List[A]): List[A] =
    foldRight2(a1,a2)((elem, acc) => Cons(elem,acc))

  def append[A](listOfList: List[List[A]]): List[A] =
    foldRight2(listOfList,Nil:List[A] )((elem, acc) => append(elem, acc) )

  def addOne(as: List[Int]): List[Int] =
    map(as)(_ + 1)

  def listToString(as: List[Double]): List[String] =
    map(as)(_.toString)

  def map[A,B](as: List[A])(f: A => B): List[B] =
    foldRight2(as, Nil: List[B])( (elem, acc) => Cons(f(elem), acc))

  def filter[A](as: List[A])(f: A => Boolean): List[A] =
    foldRight2(as, Nil: List[A])((elem, acc) => {
      if(f(elem)) {
        Cons(elem, acc)
      } else {
        acc
      }
    })

  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] =
    foldLeft(as, Nil: List[B])( (acc, elem) => append(acc, f(elem)))


  def filter2[A](as: List[A])(f: A => Boolean): List[A] =
    flatMap(as) { elem =>
      if(f(elem)) {
        List(elem)
      } else {
        Nil
      }
    }

  def constructorAdd(a1 : List[Int], a2: List[Int]) : List[Int] = {
    @tailrec
    def loop(b1 : List[Int], b2: List[Int], acc: List[Int]): List[Int] = {
      b1 match {
        case Nil => acc
        case Cons(x, xs) => loop(xs, tail(b2), Cons(x + head(b2), acc))
      }
    }
    if(length(a1) != length(a2)) {
      throw new ArrayIndexOutOfBoundsException("both list must be the same size")
    }
    reverse(loop(a1, a2, Nil))
  }

}