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
}
