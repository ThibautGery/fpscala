package chapter3

import org.specs2.mutable.Specification

import List._


class ListTest extends Specification {
  "pattern matching" >> {
    "work as expected" >> {
      val x = List(1,2,3,4,5) match {
        case Cons(x, Cons(2, Cons(4, _))) => x
        case Nil => 42
        case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
        case Cons(h, t) => h + sum(t)
        case _ => 101
      }
      x must_== 3
    }
  }
  "tail function" >> {
    "return the tail" >> {
      tail(List(1,2)) must_== List(2)
    }
  }

  "set head" >> {
    "return Nil if new is Nil" >> {
      setHead(Nil, List(1,2,3)) must_== Nil
    }

    "return the list with the head replaces" >> {
      setHead(4, List(1,2,3)) must_== List(4,2,3)
    }
  }



  "drop " >> {

    "return the same list if no item to drop" >> {
      drop(List(1,2,3, 4), 0) must_== List(1, 2, 3, 4)
    }

    "return the tail list if one item to drop" >> {
      drop(List(1,2,3, 4), 1) must_== List( 2, 3, 4)
    }

    "return the list with the head replaces" >> {
      drop(List(1,2,3, 4), 2) must_== List(3, 4)
    }
  }
}
