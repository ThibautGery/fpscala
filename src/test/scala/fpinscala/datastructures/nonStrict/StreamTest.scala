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
      Stream(1, 2, 3, 4).takeWhileAsUnfold(i => false) must_== Empty
    }

    "return the stream if dropping none" in {
      Stream(1, 2, 3, 4).takeWhile(i => true).toList must_== List(1, 2, 3, 4)
      Stream(1, 2, 3, 4).takeWhileAsUnfold(i => true).toList must_== List(1, 2, 3, 4)
    }

    "return the stream with only the first items" in {
      Stream(1, 2, 3, 4).takeWhile(i => i <= 2).toList must_== List(1, 2)
      Stream(1, 2, 3, 4).takeWhileAsUnfold(i => i <= 2).toList must_== List(1, 2)
    }
  }

  "take function" should {
    "return the empty stream if taking none" in {
      Stream(1, 2, 3, 4).take(0) must_== Empty
      Stream(1, 2, 3, 4).takeAsUnfold(0) must_== Empty
    }

    "return the the stream if taking more" in {
      Stream(1, 2, 3, 4).take(1000).toList must_== List(1, 2, 3, 4)
      Stream(1, 2, 3, 4).takeAsUnfold(1000).toList must_== List(1, 2, 3, 4)
    }

    "return the stream without the last items" in {
      Stream(1, 2, 3, 4).take(2).toList must_== List(1, 2)
      Stream(1, 2, 3, 4).takeAsUnfold(2).toList must_== List(1, 2)
    }
  }

  "the exists function" should {
    "return false if the element is not present" in {
      Stream(1, 2, 3, 4).exists(x => x > 4 ) must_== false
    }

    "return true if the element is present" in {
      Stream(1, 2, 3, 4).exists(x => x < 4) must_== true
    }

    "be lazy" in {
      Stream.ones.exists(_ % 2 != 0) must_== true
    }
  }


  "the forAll function" should {
    "return false if the function is not always true" in {
      Stream(1, 2, 3, 4).forAll(x => x > 2 ) must_== false
    }

    "return true if the element is present" in {
      Stream(1, 2, 3, 4).forAll(x => x < 5) must_== true
    }

    "be lazy" in {
      Stream.ones.forAll( _ != 1) must_== false
    }
  }

  "the headOption function" should {
    "return None if no element" in {
      Stream.empty.headOption must_== None
    }

    "return Some if the stream is not empty" in {
      Stream(1, 2, 3, 4).headOption must_== Some(1)
    }

    "be lazy" in {
      Stream.ones.headOption must_== Some(1)
    }
  }

  "the map function" should {
    "return Empty if no element" in {
      Stream.empty[Int].map( _ + 1) must_== Empty
      Stream.empty[Int].mapAsUnfold( _ + 1) must_== Empty
    }

    "return the mapped element if the stream is not empty" in {
      Stream(1, 2, 3, 4).map( _ + 1).toList must_== List(2, 3, 4, 5)
      Stream(1, 2, 3, 4).mapAsUnfold( _ + 1).toList must_== List(2, 3, 4, 5)
    }

    "be lazy" in {
      Stream.ones.map( _ + 1).take(5).toList must_== List(2, 2, 2, 2, 2)
      Stream.ones.mapAsUnfold( _ + 1).take(5).toList must_== List(2, 2, 2, 2, 2)
    }
  }

  "the filter function" should {
    "return Empty if no element" in {
      Stream.empty[Int].filter( i => true) must_== Empty
    }

    "return the matching element if the stream is not empty" in {
      Stream(1, 2, 3, 4).filter( i =>  i % 2 == 0).toList must_== List(2, 4)
    }

    "be lazy" in {
      Stream.ones.filter(_ => true).take(5).toList must_== List(1, 1, 1, 1, 1)
    }
  }

  "the append function" should {
    "return Empty if both stream are empty" in {
      Stream.empty[Int].append(Stream.empty[Int]) must_== Empty
    }

    "return the first stream  if the second one is empty" in {
      Stream(1, 2, 3, 4).append(Stream.empty[Int]).toList must_== List(1, 2, 3, 4)
    }

    "return the second stream  if the first one is empty" in {
      Stream.empty[Int].append(Stream(1, 2, 3, 4)).toList must_== List(1, 2, 3, 4)
    }

    "return the concatenated stream" in {
      Stream(1, 2, 3, 4).append(Stream(5, 6, 7, 8)).toList must_== List(1, 2, 3, 4, 5, 6, 7, 8)
    }

    "be lazy" in {
      Stream.ones.map(_ + 2).append(Stream.ones).take(5).toList must_== List(3, 3, 3, 3, 3)
    }
  }

  "the flatmap function" should {
    "return Empty if no element" in {
      Stream.empty[Int].flatMap(i =>  Stream(i, i)) must_== Empty
    }

    "return the mapped element if the stream is not empty" in {
      Stream(1, 2, 3, 4).flatMap(i =>  Stream(i, i)) .toList must_== List(1, 1, 2, 2, 3, 3, 4, 4)
    }

    "be lazy" in {
      Stream(1, 2, 3, 4).flatMap(i =>  Stream.ones().map(_ + i)).take(5).toList must_== List(2, 2, 2, 2, 2)
    }
  }

  "the from function" should {
    "return the list of following integer" in {
      Stream.from(45).take(4).toList must_== List(45, 46, 47, 48)
    }
  }

  "the fibs function" should {
    "return the list of following integer" in {
      Stream.fibs.take(6).toList must_== List(0, 1, 1, 2, 3, 5)
    }
  }

  "the zipWith function" should {
    "return Empty if no element for the first one" in {
      Stream.empty[Int].zipWith(Stream("toto", "tata"))((a, b) => {a + b.length}) must_== Empty
    }

    "rreturn Empty if no element for the second one" in {
      Stream(1, 2, 3).zipWith(Stream.empty[String])((a, b) => {a + b.length}) must_== Empty
    }

    "return the combine data" in {
      Stream(1, 2, 3).zipWith(Stream("toto", "ta"))((a, b) => {a + b.length}).toList must_== List(5, 4)
    }
  }

  "the zipAll function" should {
    "return None if no element for the first one" in {
      Stream.empty[Int].zipAll(Stream("toto", "tata")).toList must_== List((None, Some("toto")), (None, Some("tata")))
    }

    "eturn None if no element for the second one" in {
      Stream(1, 2, 3).zipAll(Stream.empty[String]).toList must_== List((Some(1), None), (Some(2), None), (Some(3), None))
    }

    "return the combine data" in {
      Stream(1, 2, 3).zipAll(Stream("toto", "tata")).toList must_== List((Some(1), Some("toto")), (Some(2), Some("tata")), (Some(3), None))
    }
  }

}
