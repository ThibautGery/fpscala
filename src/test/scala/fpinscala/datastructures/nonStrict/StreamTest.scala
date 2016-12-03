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

    "return the stream without the first item" in {
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
}
