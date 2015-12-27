package chapter4

import org.specs2.mutable.Specification


class EitherTest extends Specification {
  "map" >> {
    "of Left return the same Left" >> {
      Left("Error").map(a => a) must_== Left("Error")
    }

    "of Right returnt the Right with the updated value" >> {
      Right("Not an error").map(a => a.length) must_== Right(12)
    }
  }
}
