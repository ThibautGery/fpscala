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

  "takeWhile" >> {
    "none return empty" >> {
      Stream(1, 2, 3).takeWhile((a) => false).toList must_== Nil
    }

    "a number return the correct number" >> {
      Stream(1, 2, 3, -1, -2).takeWhile(a => a > 0).toList must_== List(1, 2, 3)
    }
  }

  "forAll" >> {
    "return true if all element are valid" >>{
      Stream(1, 2, 3, 4).forAll(a => a > 0) must_== true
    }

    "return false if one element is not valid" >>{
      Stream(1, 2, -3, 4).forAll(a => a > 0) must_== false
    }
  }
}
