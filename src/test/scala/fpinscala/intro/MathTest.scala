package fpinscala.intro

import org.specs2.mutable.Specification


class MathTest extends Specification {

  "mean" should {
    "return None for Nil" in {
      Math.mean(Nil) must_== None
    }

    "return mean for integer list" in {
      Math.mean(List(1, 2, 3)) must_== Some(2)
    }
  }

  "variance per item" should {
    "return the correct value " in {
      Math.variancePerItem(2)(1) must_== 1
      Math.variancePerItem(3)(1) must_== 4
    }
  }


  "variance" should {
    "return None for Nil" in {
      Math.variance(Nil) must_== None
    }

    "return the computed variance" in {
      Math.variance(List(600, 470, 170, 430, 300)) must_== Some(21704)
    }
  }
}
