package chapter5

import scala.annotation.tailrec


sealed trait Stream[+A] {
  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h, t) => Some(h())
  }

  def headOption2: Option[A] =
    foldRight[Option[A]](None)((elem, _) => Some(elem))

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

  def drop(n: Int): Stream[A] = {
    @tailrec
    def loop(stream: Stream[A], n: Int): Stream[A] = {
      (n, stream) match {
        case (0, _) => stream
        case (_, Empty) => throw new NoSuchElementException("impossible to drop more element than the stream size")
        case (_, Cons(h, t)) => loop(t(), n - 1)
      }
    }
    loop(this, n)
  }

  def takeWhile(p: A => Boolean): Stream[A] = {
    @tailrec
    def loop(stream: Stream[A], acc: Stream[A], p: A => Boolean): Stream[A] = {
      stream match {
        case Cons(h, t) if p(h()) => loop(t(),Cons(h, () => acc), p)
        case _ => acc
      }
    }
    Stream.fromList(loop(this, Empty, p).toList.reverse)
  }

  def takeWhile2(p: A => Boolean): Stream[A] =
    this.foldRight(Stream.empty[A])((elem, acc) => {
      if(p(elem)) {
        Stream.cons(elem, acc)
      } else {
        Stream.empty[A]
      }
    })

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b)


  def foldRight[B](z: => B)(f: (A, => B) => B): B =
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f))
      case _ => z
    }

  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a, b) => p(a) && b)

  def map[B](f: A => B): Stream[B] =
    foldRight(Stream.empty[B])((a, acc) => Stream.cons(f(a), acc))

  def filter(f: A => Boolean): Stream[A] =
    foldRight(Stream.empty[A])((a, acc) => if(f(a)) Stream.cons(a, acc) else acc)

  def append[S >: A](s2: Stream[S]): Stream[S] =
    foldRight(s2)((el, acc) => Stream.cons(el, acc))



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