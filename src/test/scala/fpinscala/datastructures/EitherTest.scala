package fpinscala.datastructures

import org.specs2.mutable.Specification

class EitherTest extends Specification {
  "The map function" should {
    "return Left for Left" in {
      val either:Either[String, Int] = Left("toto")
      either.map(i => i + 2) must_== Left("toto")
    }

    "return the value modified by the function" in {
      val either:Either[String, Int] = Right(4)
      either.map(i => i + 2) must_== Right(6)
    }
  }

  "The flatmap function" should {
    "return Left for Left" in {
      val either:Either[String, Int] = Left("toto")
      either.flatMap(i => Right(i + 2)) must_== Left("toto")
    }

    "return the value modified by the function" in {
      val either: Either[String, Int] = Right(4)
      either.flatMap(i => Right(i + 2)) must_== Right(6)
      either.flatMap(i => Left("raté")) must_== Left("raté")
    }
  }

  "The orElse function" should {
    "return the else option for Left" in {
      val either:Either[String, Int] = Left("toto")
      either.orElse(Left("ata")) must_== Left("ata")
      either.orElse(Right(6)) must_== Right(6)

    }

    "return the value modified by the function" in {
      val either: Either[String, Int] = Right(4)
      either.orElse(Left("ata")) must_==  Right(4)
    }
  }
}
