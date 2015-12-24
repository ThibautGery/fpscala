package chapter3

import org.specs2.mutable.Specification

import List.{sum, tail }


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
      tail(Cons(1, Cons(2, Nil))) must_== Cons(2, Nil)
    }
  }
}
