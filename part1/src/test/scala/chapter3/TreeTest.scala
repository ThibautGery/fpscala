package chapter3

import chapter3.Tree._
import org.specs2.mutable.Specification


class TreeTest extends Specification {
  "size" >> {
    "return 1 if one leaf" >> {
      Tree.size(Leaf(1)) must_== 1
    }

    "return the size of tree" >> {
      Tree.size(Branch(Leaf(1), Branch(Leaf(2), Leaf(3)))) must_== 5
      Tree.size(Branch(Branch(Leaf(4), Leaf(5)), Branch(Leaf(2), Leaf(3)))) must_== 7
    }
  }

  "max" >> {
    "return value of leaf if one leaf" >> {
      max(Leaf(4)) must_== 4
    }

    "return the size of tree" >> {
      max(Branch(Leaf(1), Branch(Leaf(2), Leaf(3)))) must_== 3
      max(Branch(Branch(Leaf(4), Leaf(5)), Branch(Leaf(2), Leaf(3)))) must_== 5
    }
  }

  "max depth" >> {
    "return 1 if one leaf" >> {
      maxDepth(Leaf(4)) must_== 1
    }

    "return value of leaf if one leaf" >> {
      maxDepth(Branch(Leaf(4), Leaf(2))) must_== 2
    }

    "return max depth of a tree" >> {
      maxDepth(Branch(Leaf(1), Branch(Leaf(2), Leaf(3)))) must_== 3
      maxDepth(Branch(Branch(Leaf(4), Leaf(5)), Branch(Leaf(2), Branch(Leaf(6), Leaf(7))))) must_== 4
    }
  }

  "map" >> {
    "can count the char in a node" >> {
      Tree.map(Branch(Leaf("abc"), Leaf("poiuyt")))(a => a.length) must_== Branch(Leaf(3), Leaf(6))
      Tree.map(Branch(Branch(Leaf("abc"), Leaf("poiuyt")), Leaf("poiuyt")))(a => a.length) must_== Branch(Branch(Leaf(3), Leaf(6)), Leaf(6))
    }
  }

  "using fold" >> {
    "max depth" >> {
      "return 1 if one leaf" >> {
        maxDepth2(Leaf(4)) must_== 1
      }

      "return 2 if one branch with 2 leaves" >> {
        maxDepth2(Branch(Leaf(4), Leaf(2))) must_== 2
      }

      "return max depth of a tree" >> {
        maxDepth2(Branch(Branch(Leaf(2), Leaf(3)), Leaf(1))) must_== 3
        maxDepth2(Branch(Leaf(1), Branch(Leaf(2), Leaf(3)))) must_== 3
        maxDepth2(Branch(Branch(Leaf(4), Leaf(5)), Branch(Leaf(2), Branch(Leaf(6), Leaf(7))))) must_== 4
      }
    }

    "map" >> {
      "can count the char in a node" >> {
        Tree.map2(Branch(Leaf("abc"), Leaf("poiuyt")))(a => a.length) must_== Branch(Leaf(3), Leaf(6))
        Tree.map2(Branch(Branch(Leaf("abc"), Leaf("poiuyt")), Leaf("poiuyt")))(a => a.length) must_== Branch(Branch(Leaf(3), Leaf(6)), Leaf(6))
      }
    }

    "max" >> {
      "return value of leaf if one leaf" >> {
        max2(Leaf(4)) must_== 4
      }

      "return the size of tree" >> {
        max2(Branch(Leaf(1), Branch(Leaf(2), Leaf(3)))) must_== 3
        max2(Branch(Branch(Leaf(4), Leaf(5)), Branch(Leaf(2), Leaf(3)))) must_== 5
      }
    }

    "size" >> {
      "return 1 if one leaf" >> {
        Tree.size2(Leaf(1)) must_== 1
      }

      "return the size of tree" >> {
        Tree.size2(Branch(Leaf(1), Branch(Leaf(2), Leaf(3)))) must_== 5
        Tree.size2(Branch(Branch(Leaf(4), Leaf(5)), Branch(Leaf(2), Leaf(3)))) must_== 7
      }
    }
  }
}
