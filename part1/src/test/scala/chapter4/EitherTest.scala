package chapter4

import org.specs2.mutable.Specification


class EitherTest extends Specification {
  "map" >> {
    "of Left return the same Left" >> {
      Left("Error").map(a => a) must_== Left("Error")
    }

    "of Right return the Right with the updated value" >> {
      Right("Not an error").map(a => a.length) must_== Right(12)
    }
  }

  "flatmap" >> {
    "of Left return the same Left" >> {
      Left("Error").flatMap(a => Left(a)) must_== Left("Error")
    }

    "of Right return the Right with the updated value" >> {
      Right("Not an error").flatMap(a => Right(a.length)) must_== Right(12)
    }

    "of Right return the Left with the updated value" >> {
      Right("Not an error").flatMap(a => Left(a.length)) must_== Left(12)
    }
  }

  "orElse" >> {
    "of Left return the default" >> {
      Left("Error").orElse(Right("toto")) must_== Right("toto")

    }

    "of Right return the value" >> {
      Right("titi").orElse(Right("toto")) must_== Right("titi")

    }
  }
}
