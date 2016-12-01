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

    "return None for None" in {
      Some(2).getOrElse(4) must_== 2
    }
  }


  "The getOrElse function" should {
    "return the default value for None" in {
      val option:Option[Int] = None
      option.orElse(Some(4)) must_== Some(4)
    }

    "return None for None" in {
      Some(2).orElse(Some(4)) must_== Some(2)
    }
  }

}
