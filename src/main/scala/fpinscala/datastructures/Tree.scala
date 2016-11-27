package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {
  def max(t: Tree[Int]): Int = fold[Int, Int](t)((l,r) => l.max(r), v => v)



  def size[A](t: Tree[A]) : Int = fold[A, Int](t)((l, r) => l + r + 1, _ => 1)

  def depth[A](t: Tree[A]) : Int = fold[A, Int](t)((l, r) => l.max(r) + 1, _ => 1)

  def map[A, B](t: Tree[A])(f: A => B) : Tree[B] = fold[A, Tree[B]](t)((l, r) => Branch[B](l, r), v => Leaf(f(v)))

  def fold[A, B](t: Tree[A])(f: (B, B) => B, g: A => B): B = t match {
    case Branch(l, r) => f(fold(l)(f, g), fold(r)(f, g))
    case Leaf(v) => g(v)
  }
}