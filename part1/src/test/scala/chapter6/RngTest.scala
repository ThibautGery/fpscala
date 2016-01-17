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

  "Rng double" >> {
    "must be between 0 and 1 (not including)" >> prop { (seed: Long) =>
      RNG.double(new SimpleRNG(seed))._1 must beBetween(0.0, 1.0).excludingEnd
    }.set(minTestsOk = 500)

    "must be different than the previous one" >> prop { (seed: Long) =>
      val (rand1, gen1) = RNG.double(new SimpleRNG(seed))
      RNG.double(gen1)._1 must be_!=(rand1)
    }.set(minTestsOk = 500)
  }

  "Rng intDouble" >> {
    "must be return two different random" >> prop { (seed: Long) =>
      val (intRand, doubleRand) = RNG.intDouble(new SimpleRNG(seed))._1
      doubleRand must be_!=(("0." + Math.abs(intRand).toString).toDouble)
    }.set(minTestsOk = 500)

    "must be different than the previous one" >> prop { (seed: Long) =>
      val (rand1, gen1) = RNG.intDouble(new SimpleRNG(seed))
      val rand2 = RNG.intDouble(gen1)._1
      rand1._1 must be_!=(rand2._1)
      rand1._2 must be_!=(rand2._2)
    }.set(minTestsOk = 500)
  }

  "Rng doubleInt" >> {
    "must be return two different random" >> prop { (seed: Long) =>
      val (doubleRand, intRand) = RNG.doubleInt(new SimpleRNG(seed))._1
      doubleRand must be_!=(("0." + Math.abs(intRand).toString).toDouble)
    }.set(minTestsOk = 500)

    "must be different than the previous one" >> prop { (seed: Long) =>
      val (rand1, gen1) = RNG.doubleInt(new SimpleRNG(seed))
      val rand2 = RNG.doubleInt(gen1)._1
      rand1._1 must be_!=(rand2._1)
      rand1._2 must be_!=(rand2._2)
    }.set(minTestsOk = 500)
  }

  "Rng randIntDouble" >> {
    "must be return two different random" >> prop { (seed: Long) =>
      val (intRand, doubleRand) = RNG.randIntDouble(new SimpleRNG(seed))._1
      doubleRand must be_!=(("0." + Math.abs(intRand).toString).toDouble)
    }.set(minTestsOk = 500)

    "must be different than the previous one" >> prop { (seed: Long) =>
      val (rand1, gen1) = RNG.randIntDouble(new SimpleRNG(seed))
      val rand2 = RNG.randIntDouble(gen1)._1
      rand1._1 must be_!=(rand2._1)
      rand1._2 must be_!=(rand2._2)
    }.set(minTestsOk = 500)
  }

  "Rng randDoubleInt" >> {
    "must be return two different random" >> prop { (seed: Long) =>
      val (doubleRand, intRand) = RNG.randDoubleInt(new SimpleRNG(seed))._1
      doubleRand must be_!=(("0." + Math.abs(intRand).toString).toDouble)
    }.set(minTestsOk = 500)

    "must be different than the previous one" >> prop { (seed: Long) =>
      val (rand1, gen1) = RNG.randDoubleInt(new SimpleRNG(seed))
      val rand2 = RNG.randDoubleInt(gen1)._1
      rand1._1 must be_!=(rand2._1)
      rand1._2 must be_!=(rand2._2)
    }.set(minTestsOk = 500)
  }

  "Rng double3" >> {
    "must be return three different double" >> prop { (seed: Long) =>
      val (d1, d2, d3) = RNG.double3(new SimpleRNG(seed))._1
      d1 must be_!=(d2)
      d1 must be_!=(d3)
    }.set(minTestsOk = 500)

    "must be different than the previous one" >> prop { (seed: Long) =>
      val (rand1, gen1) = RNG.double3(new SimpleRNG(seed))
      val rand2 = RNG.double3(gen1)._1
      rand1._1 must be_!=(rand2._1)
      rand1._2 must be_!=(rand2._2)
      rand1._3 must be_!=(rand2._3)
    }.set(minTestsOk = 500)
  }


  "Rng ints" >> {
    "must be return different ints" >> prop { (seed: Long) =>
      val randomList = RNG.ints(3)(new SimpleRNG(seed))._1
      randomList.head must be_!=(randomList(1))
      randomList.head must be_!=(randomList(2))
    }.set(minTestsOk = 500)

    "must be different than the previous one" >> prop { (seed: Long) =>
      val (rand1, gen1) = RNG.ints(2)(new SimpleRNG(seed))
      val rand2 = RNG.ints(2)(gen1)._1

      rand1.head must be_!=(rand2.head)
      rand1(1) must be_!=(rand2(1))

      rand1.head must be_!=(rand2(1))
      rand2.head must be_!=(rand1(1))

    }.set(minTestsOk = 500)
  }

  "nonNegativeEven" >> {
    "must return non negative and divisible by two integer" >> prop { (seed: Long) =>
      val (rand, gen) = RNG.nonNegativeEven(new SimpleRNG(seed))
      rand must be_>=(0)
      rand % 2 must be_==(0)
    }.set(minTestsOk = 500)
  }
}
