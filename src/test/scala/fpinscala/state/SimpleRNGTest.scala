package fpinscala.state

import org.specs2.ScalaCheck
import org.specs2.mutable.Specification

class SimpleRNGTest extends Specification with ScalaCheck{
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
}
