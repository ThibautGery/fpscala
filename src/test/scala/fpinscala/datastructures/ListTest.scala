package fpinscala.datastructures

import org.specs2.mutable.Specification

class ListTest extends Specification {
  "The sum" should {
    "return 0  for an empty list" in {
      List.sum(List()) must_== 0
    }

    "return the value for list of one item" in {
      List.sum(List(12)) must_== 12
    }

    "return the sum for list" in {
      List.sum(List(12,1)) must_== 13
    }
  }

  "The product" in {
    "return 1 for an empty list" in {
      List.product(List()) must_== 1
    }

    "return 0 if one item is )" in {
      List.product(List(1, 45, 67,899,34, 0, 45)) must_== 0
    }

    "return the correct product of all the items" in {
      List.product(List(1, 2, 3, 5)) must_== 30
    }
  }

  "The tail function" should {
    "throw an exception if the list is empty" in {
      List.tail(List()) must throwA( new NoSuchElementException("The list is empty"))
    }

    "return the list except for the first element" in {
      List.tail(List(1, 2, 3)) must_== List(2, 3)
    }
  }

   "The set head function" should {
     "throw an exception if the list is empty" in {
       List.setHead(34, Nil) must throwA( new NoSuchElementException("The list is empty"))
     }

     "return the list except for the first element" in {
       List.setHead(34, List(1, 2, 3)) must_== List(34, 2, 3)
     }
   }

  "The drop function" should {
    "throw an exception if the list not big enough" in {
      List.drop(List(1, 2, 3), 4) must throwA( new NoSuchElementException("The list is empty"))
    }

    "return Nil when removing the list length" in {
      List.drop(List(1, 2, 3), 3) must_== List()
    }

    "return the last elem when removing 2 items" in {
      List.drop(List(1, 2, 3, 4), 2) must_== List(3)
    }
  }
}
