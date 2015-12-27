package chapter5

import org.specs2.mutable.Specification


class StreamTest extends Specification {
  "to list" >> {
    "of Empty return empty" >> {
      Stream().toList must_== Nil
    }

    "of stream return list" >> {
      Stream(1, 2, 3).toList must_== List(1, 2, 3)
    }
  }


  "take" >> {
    "none return empty" >> {
      Stream(1, 2, 3).take(0).toList must_== Nil
    }

    "a number return the correct number" >> {
      Stream(1, 2, 3).take(2).toList must_== List(1, 2)
    }
  }

  "drop" >> {
    "none return empty" >> {
      Stream(1, 2, 3).drop(0).toList must_==  List(1, 2, 3)
    }

    "a number return the correct number" >> {
      Stream(1, 2, 3).drop(1).toList must_== List(2, 3)
    }
  }
}
