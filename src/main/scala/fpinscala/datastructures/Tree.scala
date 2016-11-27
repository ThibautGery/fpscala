package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {
  def max(t: Tree[Int]): Int = {
    def max(t1: Tree[Int], t2: Tree[Int]): Int = (t1, t2) match {
      case (Leaf(v1), Leaf(v2)) => v1.max(v2)
      case (Branch(l, r), Leaf(v)) => max(l,r).max(v)
      case (Leaf(v), Branch(l, r)) => max(l,r).max(v)
      case (Branch(l1, r1), Branch(l2, r2)) => max(l1, r1).max(max(l2, r2))
    }

    t match {
      case Leaf(v) => v
      case Branch(t1, t2) => max(t1, t2)
    }
  }

  def size[A](t: Tree[A]) : Int = t match {
    case Branch(t1, t2) => size(t1) + size(t2) + 1
    case Leaf(_) => 1
  }
}