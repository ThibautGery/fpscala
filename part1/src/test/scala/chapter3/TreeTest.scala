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
}
