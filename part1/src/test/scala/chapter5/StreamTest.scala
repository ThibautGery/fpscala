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
      Stream(1, 2, 3).takeWhile2((a) => false).toList must_== Nil
    }

    "a number return the correct number" >> {
      Stream(1, 2, 3, -1, -2, 1).takeWhile(a => a > 0).toList must_== List(1, 2, 3)
      Stream(1, 2, 3, -1, -2, 1).takeWhile2(a => a > 0).toList must_== List(1, 2, 3)
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
  "headOption" >> {
    "return None if empty stream" >> {
      Stream.empty.headOption must_== None
      Stream.empty.headOption2 must_== None
    }

    "return Some(head) if non empty stream" >> {
      Stream(1, 2, 3).headOption must_== Some(1)
      Stream(1, 2, 3).headOption2 must_== Some(1)
    }
  }

  "map" >> {
    "return None if empty stream" >> {
      Stream.empty[Int].map[Int](a => a) must_== Stream.empty[Int]
      Stream.empty[Int].map2[Int](a => a) must_== Stream.empty[Int]
    }

    "return Some(head) if non empty stream" >> {
      Stream(1, 2, 3).map(_ * 2).toList must_== List(2, 4, 6)
      Stream(1, 2, 3).map2(_ * 2).toList must_== List(2, 4, 6)
    }
  }

  "filter" >> {
    "return None if empty stream" >> {
      Stream.empty[Int].filter(a => true) must_== Stream.empty[Int]
    }

    "return Some(head) if non empty stream" >> {
      Stream(1, 2, 3).filter(a => a % 2 != 0).toList must_== List(1, 3)
    }
  }

  "append function" >> {
    "append the second one to the first" >> {
      Stream(1, 2).append(Stream(3, 4)).toList must_== List(1, 2, 3, 4)
    }

    "return the first one if the second is empty" >> {
      Stream(1, 2).append(Stream.empty[Int]).toList must_== List(1, 2)
    }

    "return the second one if the first is empty" >> {
      Stream.empty[Int].append(Stream(1, 2)).toList must_== List(1, 2)
    }
  }

  "flatmap" >> {
    "works" >> {
      Stream(1,2,3).flatMap(i => Stream(i,i)).toList must_== List(1,1,2,2,3,3)
    }
  }


  "infinite stream" >> {
    "constant stream" >> {
      Stream.constant("toto").take(10).toList must_== List("toto", "toto", "toto", "toto", "toto", "toto", "toto", "toto", "toto", "toto")
      Stream.constant2("toto").take(10).toList must_== List("toto", "toto", "toto", "toto", "toto", "toto", "toto", "toto", "toto", "toto")
    }

    "from stream" >> {
      Stream.from(4).take(10).toList must_== List(4, 5, 6, 7, 8, 9, 10, 11, 12, 13)
      Stream.from2(4).take(10).toList must_== List(4, 5, 6, 7, 8, 9, 10, 11, 12, 13)
    }

    "fibonacci stream" >> {
      Stream.fibs().take(8).toList must_== List(0, 1, 1, 2, 3, 5, 8, 13)
      Stream.fibs2().take(8).toList must_== List(0, 1, 1, 2, 3, 5, 8, 13)
    }

    "unfold" >> {
      "empty stream" >> {
        Stream.unfold(0)(a => None) must_== Stream.empty[Int]
      }

      "decresing stream" >> {
        Stream.unfold(10)(a => if(a > 0) Some((a, a- 1)) else None).toList must_== List(10, 9, 8, 7, 6, 5, 4, 3, 2 , 1)
      }
    }
  }
}
