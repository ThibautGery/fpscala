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

  "The map2 function" should {
    "return the Left for if the first either is Left" in {
      val e1:Either[String, Int] = Left("tata")
      val e2:Either[String, String] = Right("toto")
      e1.map2(e2)((a, b) => a + b.length) must_== Left("tata")
    }

    "return the Left for if the second either is Left" in {
      val e1:Either[String, Int] = Right(4)
      val e2:Either[String, String] = Left("tata")
      e1.map2(e2)((a, b) => a + b.length) must_== Left("tata")
    }

    "return Right for is both either are right" in {
      val e1:Either[String, Int] = Right(4)
      val e2:Either[String, String] = Right("toto")
      e1.map2(e2)((a, b) => a + b.length) must_== Right(8)
    }
  }

  "The try function" should {
    "return Right if not exception are thrown" in {
      Either.Try(1) must_== Right(1)
    }

    "return Left if an exeception is thrown" in {
      val e = new Exception("toto")
      Either.Try(throw e) must_== Left(e)
    }
  }
}
