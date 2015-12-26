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

  "flat map function" >> {
    "shoud transform None in None" >> {
      None.flatMap(t => t) must_== None
    }

    "shoud return none if function is none" >> {
      Some("ezezeze").flatMap( a => None) must_== None
    }

    "shoud count the size of an string" >> {
      Some("ezezeze").flatMap( a => Some(a.length)) must_== Some(7)
    }
  }

  "get or else" >> {
    "Some of a value return the value" >> {
      Some(1).getOrElse(2) must_== 1
    }

    "None return the default" >> {
      None.getOrElse(2) must_== 2

    }
  }
}
