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

  "The maximun function" should {
    "return the value for an leaf" in {
      Tree.max(Leaf(1)) must_== 1
    }

    "return the max of the only two leaf" in {
      Tree.max(Branch(Leaf(1), Leaf(2))) must_== 2
    }

    "return the max of the leaves" in {
      Tree.max(Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(3), Leaf(4)))) must_== 4
      Tree.max(Branch(Branch(Leaf(1), Leaf(2)), Leaf(4))) must_== 4
      Tree.max(Branch(Leaf(2), Branch(Leaf(3), Leaf(4)))) must_== 4
    }
  }
}
