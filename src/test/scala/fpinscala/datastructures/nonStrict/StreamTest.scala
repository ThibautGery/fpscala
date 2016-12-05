package fpinscala.datastructures.nonStrict

import org.specs2.mutable.Specification

/**
  * Created by thibautgery on 3/12/16.
  */
class StreamTest extends Specification {

  "toList function" should {
    "convert the stream to list" in {
      Stream(1, 2, 3, 4).toList must_== List(1, 2, 3, 4)
    }
  }

  "drop function" should {
    "return the empty stream if droping more elem than the list" in {
      Stream(1, 2, 3, 4).drop(576) must_== Empty
    }

    "return the stream if dropping none" in {
      Stream(1, 2, 3, 4).drop(0).toList must_== List(1, 2, 3, 4)
    }

    "return the stream without the first item" in {
      Stream(1, 2, 3, 4).drop(2).toList must_== List( 3, 4)
    }
  }

  "dropWhile function" should {
    "return the empty stream if dropping more elem than the list" in {
      Stream(1, 2, 3, 4).dropWhile(i => true) must_== Empty
    }

    "return the stream if dropping none" in {
      Stream(1, 2, 3, 4).dropWhile(i => false).toList must_== List(1, 2, 3, 4)
    }

    "return the stream without the first item" in {
      Stream(1, 2, 3, 4).dropWhile(i => i <= 2).toList must_== List( 3, 4)
    }
  }

  "takeWhile function" should {
    "return the empty stream if dropping more elem than the list" in {
      Stream(1, 2, 3, 4).takeWhile(i => false) must_== Empty
    }

    "return the stream if dropping none" in {
      Stream(1, 2, 3, 4).takeWhile(i => true).toList must_== List(1, 2, 3, 4)
    }

    "return the stream with only the first items" in {
      Stream(1, 2, 3, 4).takeWhile(i => i <= 2).toList must_== List(1, 2)
    }
  }

  "take function" should {
    "return the empty stream if taking none" in {
      Stream(1, 2, 3, 4).take(0) must_== Empty
    }

    "return the the stream if taking more" in {
      Stream(1, 2, 3, 4).take(1000).toList must_== List(1, 2, 3, 4)
    }

    "return the stream without the last items" in {
      Stream(1, 2, 3, 4).take(2).toList must_== List(1, 2)
    }
  }

  "the exists function" should {
    "return false if the element is not present" in {
      Stream(1, 2, 3, 4).exists(x => x > 4 ) must_== false
    }

    "return true if the element is present" in {
      Stream(1, 2, 3, 4).exists(x => x < 4) must_== true
    }
  }


  "the forAll function" should {
    "return false if the function is not always true" in {
      Stream(1, 2, 3, 4).forAll(x => x > 2 ) must_== false
    }

    "return true if the element is present" in {
      Stream(1, 2, 3, 4).forAll(x => x < 5) must_== true
    }
  }

  "the headOption function" should {
    "return None if no eleemt" in {
      Stream.empty.headOption must_== None
    }

    "return Some if the stream is not empty" in {
      Stream(1, 2, 3, 4).headOption must_== Some(1)
    }
  }

  "the map function" should {
    "return Empty if no eleemt" in {
      Stream.empty[Int].map( _ + 1) must_== Empty
    }

    "return the mapped element if the stream is not empty" in {
      Stream(1, 2, 3, 4).map( _ + 1).toList must_== List(2, 3, 4, 5)
    }
  }

  "the filter function" should {
    "return Empty if no eleemt" in {
      Stream.empty[Int].filter( i => true) must_== Empty
    }

    "return the mapped element if the stream is not empty" in {
      Stream(1, 2, 3, 4).filter( i =>  i % 2 == 0).toList must_== List(2, 4)
    }
  }
}
