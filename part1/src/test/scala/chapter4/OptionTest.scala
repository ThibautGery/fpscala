package chapter4

import org.specs2.mutable.Specification


class OptionTest extends Specification {
  "map function" >> {
    "shoud transform None in None" >> {
      None.map(t => t) must_== None
    }

    "shoud count the size of an string" >> {
      Some("ezezeze").map(_.length) must_== Some(7)
    }
  }
}
