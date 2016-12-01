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

    "return None for None" in {
      Some(2).map(i => i + 2) must_== Some(4)
    }
  }

}
