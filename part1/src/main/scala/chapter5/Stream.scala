package chapter5

import scala.annotation.tailrec


sealed trait Stream[+A] {
  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h, t) => Some(h())
  }

  def toList: List[A]= {
    @tailrec
    def loop(stream: Stream[A], acc: List[A]): List[A] = {
      stream match {
        case Empty => acc
        case Cons(h, t) => loop(t(), h() :: acc)
      }
    }
    loop(this, Nil).reverse
  }

  def take(n: Int): Stream[A]= {
    @tailrec
    def loop(stream: Stream[A], acc: Stream[A], n: Int): Stream[A] = {
      (n, stream) match {
        case (0, _) => acc
        case (_, Empty) => throw new NoSuchElementException("impossible to get more element than the stream size")
        case (_, Cons(h, t)) => loop(t(),Cons(h, () => acc), n - 1)
      }
    }
    Stream.fromList(loop(this, Empty, n).toList.reverse)
  }
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]


object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

  def fromList[A](as: List[A]): Stream[A] =
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

}