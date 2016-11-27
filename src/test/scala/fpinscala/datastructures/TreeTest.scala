package fpinscala.datastructures

import org.specs2.mutable.Specification


class TreeTest extends Specification {
  "The size function" should {
    "return 1 for an leaf" in {
      Tree.size(Leaf(1)) must_== 1
    }

    "return 3 for an branch and two leave" in {
      Tree.size(Branch(Leaf(1), Leaf(1))) must_== 3
    }

    "return 5 for 3 branches and 4 leaves" in {
      Tree.size(Branch(Branch(Leaf(1), Leaf(1)), Branch(Leaf(1), Leaf(1)))) must_== 7
    }
  }
}
