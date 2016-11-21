package fpinscala.datastructures

import org.specs2.mutable.Specification

class TestSpecs extends Specification {
  "The sum" should {
    "return 0  for an empty list" in {
      List.sum(Nil) must_==(0)
    }

    "return the value for list of one item" in {
      List.sum(Cons(12, Nil)) must_==(12)
    }

    "return the sum for list" in {
      List.sum(Cons(12, Cons(1, Nil))) must_==(13)
    }
  }
}
