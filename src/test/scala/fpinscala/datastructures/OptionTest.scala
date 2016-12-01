package fpinscala.datastructures

import org.specs2.mutable.Specification

/**
  * Created by thibautgery on 1/12/16.
  */
class OptionTest extends Specification {
  "The map function" should {
    "return None for None" in {
      val option:Option[Int] = None
      option.map(i => i + 2) must_== None
    }

    "return the value modified by the function" in {
      Some(2).map(i => i + 2) must_== Some(4)
    }
  }

  "The flatmap function" should {
    "return None for None" in {
      val option:Option[Int] = None
      option.flatMap(i => Some(i + 2)) must_== None
    }

    "return None for None" in {
      Some(2).flatMap(i => Some(i + 2)) must_== Some(4)
    }
  }

  "The getOrElse function" should {
    "return the default value for None" in {
      val option:Option[Int] = None
      option.getOrElse(4) must_== 4
    }

    "return the value for Some" in {
      Some(2).getOrElse(4) must_== 2
    }
  }


  "The getOrElse function" should {
    "return the default value for None" in {
      val option:Option[Int] = None
      option.orElse(Some(4)) must_== Some(4)
    }

    "return the value for Some" in {
      Some(2).orElse(Some(4)) must_== Some(2)
    }
  }

  "The filter function" should {
    "return the None for None" in {
      val option:Option[Int] = None
      option.filter( a => a > 0 ) must_== None
    }

    "return None for Some if the function return false" in {
      Some(- 2).filter( a => a > 0 ) must_== None
    }

    "return Some for Some if the function return true" in {
      Some(45).filter( a => a > 0 ) must_== Some(45)
    }
  }

  "The map2 function" should {
    "return the None for if the first option is none" in {
      val o1:Option[Int] = None
      val o2:Option[String] = Some("toto")
      Option.map2(o1, o2)((a, b) => a + b.length) must_== None
    }

    "return the None for if the second option is none" in {
      val o1:Option[Int] = Some(3)
      val o2:Option[String] = None
      Option.map2(o1, o2)((a, b) => a + b.length) must_== None
    }

    "return Some for is both option are defined" in {
      val o1:Option[Int] = Some(3)
      val o2:Option[String] = Some("toto")
      Option.map2(o1, o2)((a, b) => a + b.length) must_== Some(7)
    }
  }

}
