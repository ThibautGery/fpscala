package fpinscala.datastructures


sealed trait Either[+E, +A] {
  def map[B](f: A => B): Either[E, B] = this match {
    case Left(e) => Left(e)
    case Right(v) => Right(f(v))
  }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
    case Left(e) => Left(e)
    case Right(v) => f(v)
  }

  def orElse[EE >: E,B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
    case Left(e) => b
    case Right(v) => Right(v)
  }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C):
  Either[EE, C] = for {
                    aa <- this
                    bb <- b
                  } yield f(aa, bb)

}
case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]


object Either {
  def Try[A](a: => A): Either[Exception, A] =
    try Right(a)
    catch { case e: Exception => Left(e)}
}