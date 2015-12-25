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
}