package chapter3

import scala.annotation.tailrec


sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  def size[A](t: Tree[A]) : Int = {
    def loop(t: Tree[A]): Int = {
      t match {
        case Leaf(_) => 1
        case Branch(a, b) => loop(a) + loop(b) + 1
      }
    }
    loop(t)
  }

  def max(t: Tree[Int]): Int = {
    def loop(t: Tree[Int]): Int = {
      t match {
        case Leaf(a) => a
        case Branch(a, b) => loop(a) max loop(b)
      }
    }
    loop(t)
  }

  def maxDepth(t: Tree[Int]): Int = {
    def loop(t: Tree[Int], acc: Int): Int = {
      t match {
        case Leaf(a) => acc + 1
        case Branch(a, b) => loop(a, acc + 1) max loop(b, acc + 1)
      }
    }
    loop(t, 0)
  }

  def map[A,B](t: Tree[A])(f: A => B): Tree[B] = {
    t match {
      case Leaf(a) => Leaf(f(a))
      case Branch(left, right) => Branch(map(left)(f), map(right)(f))
    }
  }

  def fold[A,B](t: Tree[A], z: B)(f: (A, B) => B, g: (B, B) => B): B = {
    def loop(curr: Tree[A], acc: B): B = {
      curr match {
        case Leaf(a) => f(a, acc)
        case Branch(l, r) => g(loop(l, acc), loop(r, acc))
      }
    }
    loop(t, z)
  }

  def maxDepth2(t: Tree[Int]): Int =
    fold(t, 0)((elem, acc) => acc +1, (a, b) => a.max(b) + 1)


  def map2[A,B](t: Tree[A])(f: A => B): Tree[B] =
    fold[A,Tree[B]](t, null)((a, _) => Leaf(f(a)), (b1, b2) => Branch(b1, b2))

  def max2(t: Tree[Int]): Int =
    fold(t, 0)((el, acc) => el, (a, b) => a max b)


  def size2[A](t: Tree[A]) : Int =
    fold(t, 0)((_, acc) => 1 + acc, (a, b) => a + b +1)
}