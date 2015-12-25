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

  "head function" >> {
    "return the first element" >> {
      head(List(1,2)) must_== 1
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

  "drop while" >> {

    "return the same list if no item to drop" >> {
      dropWhile(List(1,2,3, 4), (x: Int) => false) must_== List(1, 2, 3, 4)
    }

    "return the empty list if we drop all item" >> {
      dropWhile(List(1,2,3, 4), (x: Int) => true) must_== Nil
    }

    "return Nil if mepty list" >> {
      dropWhile(Nil, (x: Int) => false) must_== Nil
    }

    "return the same list if the items are not in first position" >> {
      dropWhile(List(1, -2, 3, -4), (x: Int) => x <= 0) must_==  List(1, -2, 3, -4)
    }

    "return the list without its first element given it matchs the predicate" >> {
      dropWhile(List(-1, -2, 3, -4), (x: Int) => x <= 0) must_==  List( 3, -4)
    }

  }

  "init" >> {

    "return Nil if empty list" >> {
      init(Nil) must_== Nil
    }

    "return Nil if list of one element" >> {
      init(Nil) must_== Nil
    }

    "remove the last element of a list of size 2" >> {
      init(List(1, 2)) must_== List(1)
    }

    "remove the last element of a list" >> {
      init(List(1, 2, 3, 4, 5, 6)) must_== List(1, 2, 3, 4, 5)
    }
  }

  "fold right" >> {
    "can return the same list" >> {
      foldRight(List(1,2,3), Nil:List[Int])(Cons(_,_)) must_== List(1, 2, 3)
    }
  }

  "length" >> {
    "return 0 for an empty list" >> {
      List.length(List()) must_== 0
    }

    "return the size of an list" >> {
      List.length(List(1, 2, 3, 4, 5)) must_== 5
    }
  }

  "fold left tail recursive" >> {
    "return the init value for an empty list" >> {
      foldLeft(Nil, 9)((l, acc) => acc) must_== 9
    }
    "return the init value for an empty list" >> {
      foldLeft(List(1), 9)((elem, acc) => elem + acc) must_== 10
    }

    "return the init value for an empty list" >> {
      foldLeft(List(1, 2, 3, 4), 0)((elem, acc) => elem + acc) must_== 10
    }

    "the sum using foldleft" >> {
      sum3(List(1, 2, 3, 4)) must_== 10
    }

    "the product using foldleft" >> {
      product3(List(1, 2, 3, 2)) must_== 12
    }
  }

  "the reverse function" >> {
    "return Nil if fonction is Nil" >> {
      reverse(Nil) must_== Nil
    }

    "reverse the list" >> {
      reverse(List(1, 2, 3)) must_== List(3, 2, 1)
    }
  }

  "fold right implemented with foldLeft" >> {
    "can return the same list" >> {
      foldRight2(List(1,2,3), Nil:List[Int])(Cons(_,_)) must_== List(1, 2, 3)
    }
  }
}
