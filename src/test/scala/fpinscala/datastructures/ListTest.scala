package fpinscala.datastructures

import org.specs2.mutable.Specification

class TestSpecs extends Specification {
  "The sum" should {
    "return 0  for an empty list" in {
      List.sum(List()) must_==(0)
    }

    "return the value for list of one item" in {
      List.sum(List(12)) must_==(12)
    }

    "return the sum for list" in {
      List.sum(List(12,1)) must_==(13)
    }
  }
}
