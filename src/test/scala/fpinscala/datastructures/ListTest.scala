package fpinscala.datastructures

import org.specs2.mutable.Specification

class ListTest extends Specification {
  "The sum" should {
    "return 0  for an empty list" in {
      List.sum(List()) must_== 0
      List.sum2(List()) must_== 0
      List.sum3(List()) must_== 0
    }

    "return the value for list of one item" in {
      List.sum(List(12)) must_== 12
      List.sum2(List(12)) must_== 12
      List.sum3(List(12)) must_== 12
    }

    "return the sum for list" in {
      List.sum(List(12,1)) must_== 13
      List.sum2(List(12,1)) must_== 13
      List.sum3(List(12,1)) must_== 13
    }
  }

  "The product" in {
    "return 1 for an empty list" in {
      List.product(List()) must_== 1
      List.product2(List()) must_== 1
      List.product3(List()) must_== 1
    }

    "return 0 if one item is )" in {
      List.product(List(1, 45, 67,899,34, 0, 45)) must_== 0
      List.product2(List(1, 45, 67,899,34, 0, 45)) must_== 0
      List.product3(List(1, 45, 67,899,34, 0, 45)) must_== 0
    }

    "return the correct product of all the items" in {
      List.product(List(1, 2, 3, 5)) must_== 30
      List.product2(List(1, 2, 3, 5)) must_== 30
      List.product3(List(1, 2, 3, 5)) must_== 30
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
      List.drop(List(1, 2, 3, 4), 2) must_== List(3, 4)
    }
  }

  "The dropwhile function" should {
    "return the full list when the predicate is always false" in {
      List.dropWhile(List(1, 2, 3), (a:Int) => false) must_== List(1, 2, 3)
    }

    "return nil when the list is empty" in {
      List.dropWhile(Nil, (a:Int) => true) must_== Nil
    }

    "return the element when it does't match anymore" in {
      List.dropWhile(List(1, 2, 3), (a:Int) => a <=2) must_== List(3)
    }
  }

  "The append function" should {
    "return a2 when a1 is Nil" in {
      List.append(Nil, List(1, 2)) must_== List(1, 2)
    }

    "return a1 when a2 is Nil" in {
      List.append(List(1, 2), Nil) must_== List(1, 2)
    }

    "return a1 then a2" in {
      List.append(List(1, 2), List(3, 4)) must_== List(1, 2, 3, 4)
    }
  }

  "The init function" should {
    "return Nil for an empty list" in {
      List.init(Nil) must_== Nil
    }

    "return Nil for an list of one elem" in {
      List.init(List(1)) must_== Nil
    }

    "return all the element but the last" in {
      List.init(List(1, 2, 3, 4)) must_== List(1, 2, 3)
    }
  }

  "The length" should {
    "return 0  for an empty list" in {
      List.length(List()) must_== 0
    }

    "return the 1 for list of one item" in {
      List.length(List(12)) must_== 1
    }

    "return the length of the lit" in {
      List.length(List(12,1, 45, 23)) must_== 4
    }
  }

  "The reverse function" should {
    "return Nil of the the list is Nil" in {
      List.reverse(Nil) must_== Nil
    }

    "return the same list if the list is of size 1" in {
      List.reverse(List(4)) must_== List(4)
      List.reverse(List(34)) must_== List(34)
    }

    "return reverse the list" in {
      List.reverse(List(1, 2, 3, 4, 5, 6)) must_== List(6, 5, 4, 3, 2, 1)
    }
  }

  "The flatten function" should {

    "return the Nil if empty" in {
      List.flatten(Nil) must_== Nil
    }

    "return the first element if one elem" in {
      List.flatten(List(List(1, 2, 3))) must_== List(1, 2, 3)
    }

    "return a flatten list" in {
      List.flatten(List(List(1, 2, 3), List(4, 5, 6))) must_== List(1, 2, 3, 4 , 5, 6)
    }
  }

  "The add One function" should {
    "add one" in {
      List.addOne(Nil) must_== Nil
      List.addOne(List(1, 2, 3)) must_== List(2, 3, 4)
    }
  }

  "The double to String function" should {
    "convert the double to string" in {
      List.doubleToString(Nil) must_== Nil
      List.doubleToString(List(1, 2, 3)) must_== List("1.0", "2.0", "3.0")
    }
  }

  "The filter function" should {
    "return all the list when the condition is always true" in {
      List.filter(List(1, 2, 3, 4))(x => true) must_== List(1, 2, 3, 4)
    }

    "filter the elem given the predicate" in {
      List.filter(List(1, 2, 3, 4))(_ % 2 == 0) must_== List(2, 4)
    }
  }

  "The flatmap function" should {
    "return Nil when Nil" in {
      List.flatMap(Nil)(i => List(i,i)) must_== Nil
    }

    "flatmap the shit of out of the list" in {
      List.flatMap(List(1,2,3))(i => List(i,i)) must_== List(1,1,2,2,3,3)
      List.flatMap(List(1,2,3))(i => List(i, i +1)) must_== List(1,2,2,3,3,4)
    }
  }

  "The addTwoList method" should {
    "return Nil if the second is empty" in {
      List.addTwoList(List(1, 2, 3), Nil) must_== Nil
    }

    "return Nil if the first is empty" in {
      List.addTwoList(Nil, List(1, 2, 3)) must_== Nil
    }

    "add each other element in the list" in {
      List.addTwoList(List(4, 5, 6), List(1, 2, 3)) must_== List(5, 7, 9)
      List.addTwoList(List(4, 5, 6), List(1, 2, 3, 4)) must_== List(5, 7, 9)
      List.addTwoList(List(4, 5, 6, 7), List(1, 2, 3)) must_== List(5, 7, 9)
    }
  }

  "The hasSubsequence function" should {
    "return false when they is no subsequence" in {
      List.hasSubsequence(List(1, 2, 3), List(4, 5)) must_== false
      List.hasSubsequence(List(1, 2, 4, 3, 6), List(1, 3)) must_== false
    }

    "return true when the subsequence equal the list" in {
      List.hasSubsequence(List(1, 2, 3), List(1, 2, 3)) must_== true
    }

    "return true when there is the subsequence is the list" in {
      List.hasSubsequence(List(1, 2, 3), List(3)) must_== true
      List.hasSubsequence(List(1, 2, 3), List(2, 3)) must_== true
      List.hasSubsequence(List(1, 2, 3, 4, 2, 3, 4), List(2, 3, 4)) must_== true
    }
  }

}
