package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {
  def size[A](t: Tree[A]) : Int = t match {
    case Branch(t1, t2) => size(t1) + size(t2) + 1
    case Leaf(_) => 1
  }
}