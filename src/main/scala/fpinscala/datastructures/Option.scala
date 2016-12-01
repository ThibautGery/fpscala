package fpinscala.datastructures

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case Some(v) => Some(f(v))
    case None => None
  }

  def flatMap[B](f: A => Option[B]): Option[B] = this match {
    case Some(v) => f(v)
    case None => None
  }

  def getOrElse[B >: A](default: => B): B = this match {
    case Some(v) => v
    case None => default
  }

  def orElse[B >: A](ob: => Option[B]): Option[B] = this match {
    case Some(v) => Some(v)
    case None => ob
  }

  def filter(f: A => Boolean): Option[A] = this match {
    case Some(v) if f(v) => Some(v)
    case _ => None
  }

  def lift[A,B](f: A => B): Option[A] => Option[B] = _ map f
}
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]


object Option {

  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = (a, b) match {
    case (None, _) => None
    case (_, None) => None
    case (Some(a), Some(b)) => Some(f(a, b))
  }
}