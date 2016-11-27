package fpinscala.datastructures

import scala.annotation.tailrec


sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  @tailrec
  def dropWhile[A](list: List[A], f: A => Boolean): List[A]= list match {
    case Nil => Nil
    case Cons(x, xs) if f(x) => dropWhile(xs, f)
    case _ => list
  }

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

  def append[A](a1: List[A], a2: List[A]): List[A] = foldRight(a1, a2)((x, acc) => Cons(x, acc))

  def init[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(a, Nil) => Nil
    case Cons(a, t) => Cons(a, init(t))
  }


  def sum2(ns: List[Int]) = foldRight(ns, 0)((x,y) => x + y)
  def product2(ns: List[Double]) = foldRight(ns, 1.0)(_ * _)

  def sum3(ns: List[Int]) = foldRight(ns, 0)((x,y) => x + y)
  def product3(ns: List[Double]) = foldRight(ns, 1.0)(_ * _)

  def length[A](as: List[A]): Int = foldRight(as, 0)((item, acc) => acc + 1)

  def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B): B = {
    @tailrec
    def loop(ass: List[A], acc: B): B = ass match {
      case Nil => acc
      case Cons(x, xs) => loop(xs, f(acc, x))
    }
    loop(as, z)
  }

  def reverse[A](list: List[A]): List[A] = foldLeft[A, List[A]](list, Nil)((acc, item) =>  Cons(item, acc))

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = foldLeft(reverse(as), z)((b,a) => f(a,b))

  def flatten[A](ll: List[List[A]]): List[A] = foldLeft[List[A], List[A]](ll, Nil)((acc, x) => append(acc, x))

  def addOne(l: List[Int]): List[Int] = map(l)(a => a + 1 )

  def doubleToString(l: List[Double]): List[String] = map(l)(a => a.toString)

  def map[A,B](as: List[A])(f: A => B): List[B] = foldRight[A, List[B]](as, Nil)((x, acc) => Cons(f(x), acc))

  def filter[A](as: List[A])(f: A => Boolean): List[A] =
    flatMap[A, A](as)( x => if(f(x)) List(x) else Nil )

  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] =
    foldRight[A, List[B]](as, Nil)((x, acc) => {
      List.append(f(x), acc)
    })

  def addTwoList(a1: List[Int], a2: List[Int]): List[Int] = {
    @tailrec
    def loop(a1: List[Int], a2: List[Int], acc: List[Int]): List[Int] = {
      (a1, a2) match {
        case (Nil, _) => acc
        case (_, Nil) => acc
        case (Cons(x, xs), Cons(y, ys)) => loop(xs, ys, Cons(x + y, acc))
      }
    }
    List.reverse(loop(a1, a2, Nil))
  }
}