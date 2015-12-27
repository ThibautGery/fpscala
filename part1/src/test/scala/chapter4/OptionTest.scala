package chapter4

import org.specs2.mutable.Specification
import Option._

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

  "filter" >> {
    "Some of a value with good filter" >> {
      Some(1).filter(a => true) must_== Some(1)
    }

    "Some of a value with bad filter" >> {
      Some(1).filter(a => false) must_== None
    }

    "None return None" >> {
      None.filter(a => true) must_== None
    }
  }

  "or else" >> {
    "Some of a value return the value" >> {
      Some(1).orElse(Some(2)) must_== Some(1)
    }

    "None return the default" >> {
      None.orElse(Some(2)) must_== Some(2)
    }
  }

  "map2" >> {
    "return None if one None" >> {
      map2[Int, Int, Int](None, Some(1))((a, b) => a + b) must_== None
      map2[Int, Int, Int](Some(1), None)((a, b) => a + b) must_== None
    }

    "return the computed value if two some" >> {
      map2[Int, Int, Int](Some(1), Some(1))((a, b) => a + b) must_== Some(2)
    }
  }

  "sequence" >> {
    "return None if one is None" >> {
      sequence(List[Option[Int]](None, Some(2))) must_== None
    }

    "return Some of list if only Some in elements" >> {
      sequence(List[Option[Int]](Some(1), Some(2))) must_== Some(List(1, 2))
    }
  }

  "traverse" >> {
    "return None if one is None" >> {
      traverse(List[Option[Int]](None, Some(2)))(i => i.map(_ *2)) must_== None
    }

    "return Some of list if only Some in elements" >> {
      traverse(List[Option[Int]](Some(1), Some(2)))(i => i.map(_ *2)) must_== Some(List(2, 4))
    }
  }
}
