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
      val (randomNumber1, _) = RNG.double(rng)
      val (randomNumber2, _) = RNG.doubleWithMap(rng)
      randomNumber1 must be_>=(0.0) and be_<(1.0)
      randomNumber2 must be_>=(0.0) and be_<(1.0)
      randomNumber1 must_== randomNumber2
    }.set(minTestsOk = 200, workers = 3)
  }

  "The intDouble function" should {
    "return a double superior to 0 and strictly inferior to 1 and a different integer" in prop { (seed: Long) =>
      val rng = SimpleRNG(seed)
      val ((int, double), _) = RNG.intDouble(rng)
      double must be_>=(0.0) and be_<(1.0)
    }
  }

  "The doubleInt function" should {
    "return a double superior to 0 and strictly inferior to 1 and a different integer" in prop { (seed: Long) =>
      val rng = SimpleRNG(seed)
      val ((double, int), _) = RNG.doubleInt(rng)
      double must be_>=(0.0) and be_<(1.0)
    }
  }

  "The double3 function" should {
    "return a double superior to 0 and strictly inferior to 1 and a different integer" in prop { (seed: Long) =>
      val rng = SimpleRNG(seed)
      val ((v1, v2, v3), _) = RNG.double3(rng)
      v1 must be_>=(0.0) and be_<(1.0)
      v2 must be_>=(0.0) and be_<(1.0)
      v3 must be_>=(0.0) and be_<(1.0)
      v1 must_!= v2
      v1 must_!= v3
    }
  }

  "The ints function" should {
    "generate a list of distinct integer"  in prop { (seed: Long) =>
      val length = 4
      val rng = SimpleRNG(seed)
      val (listOfInts, _) = RNG.ints(length)(rng)
      listOfInts must haveLength(length)
      listOfInts.toSet must haveLength(length)
    }
  }
}
