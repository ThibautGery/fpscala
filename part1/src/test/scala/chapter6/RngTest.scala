package chapter6

import org.specs2.mutable.Specification
import org.specs2.ScalaCheck

class RngTest extends Specification with ScalaCheck {
  "Rng nonNegativeInt" >> {
    "must be positive" >> prop { (seed: Long) =>
      RNG.nonNegativeInt(new SimpleRNG(seed))._1 must be_>=(0)
    }.set(minTestsOk = 500)

    "must be different than the previous one" >> prop { (seed: Long) =>
      val (rand1, gen1) = RNG.nonNegativeInt(new SimpleRNG(seed))

      RNG.nonNegativeInt(gen1)._1 must be_!=(rand1)
    }.set(minTestsOk = 500)

  }

}
