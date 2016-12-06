package fpinscala.datastructures.nonStrict

import scala.annotation.tailrec

sealed trait Stream[+A] {
  final def drop[B >: A](n: Int): Stream[B] = (this, n) match {
    case (_, 0) => this
    case (Empty, _) => Stream.empty
    case (Cons(x, xs), i) => xs().drop( i - 1)
  }

  final def dropWhile[B >: A](f: B => Boolean): Stream[B] = this match {
    case Cons(x, xs) if f(x()) => xs().dropWhile(f)
    case _ => this
  }

  def take[B >: A](n: Int): Stream[B] = (this, n) match {
    case (_, 0) => Stream.empty
    case (Empty, _) => Stream.empty
    case (Cons(x, xs), i) => Stream.cons(x(), xs().take(i - 1))
  }

  def takeAsUnfold[B >: A](n: Int): Stream[B] = Stream.unfold[B, (Stream[B], Int)]((this, n)) {
    case (_, 0) => None
    case (Empty, _) => None
    case (Cons(h, t), i) => Some((h(), (t(), i - 1)))
  }

  def takeWhile[B >: A](f: B => Boolean): Stream[B] = foldRight(Stream.empty[B])((i, acc) => {
    if(f(i)) Stream.cons(i, acc) else Stream.empty[B]
  })

  def takeWhileAsUnfold[B >: A](f: B => Boolean): Stream[B] = Stream.unfold[B, Stream[B]](this) {
    case Cons(h, t) if f(h()) => Some((h(), t()))
    case _ => None
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(h, t) => f(h(), t().foldRight(z)(f))
    case _ => z
  }

  def headOption: Option[A] = foldRight[Option[A]](None)((i, _) => Some(i))

  def exists(p: A => Boolean): Boolean = foldRight(false)((i, acc) => p(i) || acc)

  def forAll(p: A => Boolean): Boolean = foldRight(true)((i, acc) => p(i) && acc)

  def toList[B >: A]: List[B] = {
    @tailrec
    def loop(s: Stream[B], acc: List[B]):List[B] = s match {
      case Empty => acc
      case Cons(x, xs) => loop(xs(), x() :: acc)
    }
    loop(this, Nil).reverse
  }

  def map[B](f: A => B): Stream[B] = foldRight(Stream.empty[B])((i, acc) => Stream.cons(f(i), acc))
  def mapAsUnfold[B](f: A => B): Stream[B] = Stream.unfold(this) {
    case Empty => None
    case Cons(h, t) => Some((f(h()), t()))
  }

  def filter(f: A => Boolean): Stream[A] = foldRight(Stream.empty[A]){ (i, acc) =>
    if(f(i)) Stream.cons(i, acc)
    else acc
  }

  def append[B >: A](s2: Stream[B]): Stream[B] = foldRight(s2)((i, acc) => Stream.cons(i, acc))

  def flatMap[B](f: A => Stream[B]): Stream[B] = foldRight(Stream.empty[B])((i, acc) => f(i) append acc )
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

  def ones(): Stream[Int] = constant(1)

  def constant[A](a: A): Stream[A] = unfold(a)(x => Some(x, x))

  def from(a: Int): Stream[Int] = unfold(a)(x => Some(x, x + 1))

  def fibs: Stream[Int] = unfold((0, 1))(x => {
    val (curr, next) = x
    Some(curr, (next, curr + next))
  })

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    def loop(i: S): Stream[A] = {
      f(i) match {
        case None => empty
        case Some((value, acc)) => cons(value, loop(acc))
      }
    }
    loop(z)
  }

}