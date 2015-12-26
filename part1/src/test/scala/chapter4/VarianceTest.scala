package chapter4

import org.specs2.mutable.Specification
import Variance._

class VarianceTest extends Specification {
  "ecart type" >> {
    "is 0 if value equal mean" >> {
      ecartType(1.0, 1.0) must_== Some(0.0)
    }

    "is 1 if value = 1 and mean = 0" >> {
      ecartType(1.0, 0) must_== Some(1.0)
    }

    "power the difference" >> {
      ecartType(2.0, 0) must_== Some(4.0)
      ecartType(-2.0, 0) must_== Some(4.0)
    }
  }

  "variance function" >> {
    "compute the variance" >> {
      variance(Seq(2.0, -2.0)) must_== Some(4.0)
      variance(Seq(2.0, 2.0, -2.0, -2.0)) must_== Some(4.0)
    }
  }
}
