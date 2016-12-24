package fpinscala.state

import org.specs2.ScalaCheck
import org.specs2.mutable.Specification
import org.specs2.mock.Mockito


class SimpleRNGTest extends Specification with ScalaCheck with Mockito {
  "this is a specific property" >> prop { (a: Int, b: Int) =>
    (a + b) must_== (b + a)
  }


  "The nextInt function" should {
    "return the same value if called with the same seed" in prop { (seed: Long) =>
      val rngRoot = SimpleRNG(seed)
      val (rand1, rng1) = rngRoot.nextInt
      val (rand2, _) = rngRoot.nextInt
      val (rand3, _) = rng1.nextInt

      rand1 must_== rand2
      rand1 must_!= rand3
    }.set(minTestsOk = 200, workers = 3)
  }

  "The nonNegativeInt function" should {
    "return the only positive value" in prop { (seed: Long) =>
      val rng = SimpleRNG(seed)
      val (randomNumber, _) = RNG.nonNegativeInt(rng)
      randomNumber must be_>=(0)
    }.set(minTestsOk = 200, workers = 3)

    "return a positive value if the value generated is Int.MinValue" in {
      val rng = mock[RNG]
      rng.nextInt returns((Int.MinValue, SimpleRNG(34)))
      val (randomNumber, _) = RNG.nonNegativeInt(rng)
      randomNumber must be_>=(0)
    }
  }

  "The double function" should {
    "return a double superior to 0 and strictly inferior to 1" in prop { (seed: Long) =>
      val rng = SimpleRNG(seed)
      val (randomNumber, _) = RNG.double(rng)
      randomNumber must be_>=(0.0)
      randomNumber must be_<(1.0)
    }.set(minTestsOk = 200, workers = 3)
  }
}
