package fpinscala.intro

import org.specs2.mutable.Specification



class BasicTest extends Specification {
  "Fibonacci" should {
    "return an empty list when asking 0 number" in {
      Basics.fib(0) must have length 0
    }

    "return 0 list when asking 1 number" in {
      Basics.fib(1) must have length 1
      Basics.fib(1) must contain(0)
    }

    "return 0 and 1 list when asking 2 number" in {
      Basics.fib(2) must have length 2
      Basics.fib(2) must contain(0, 1)
    }

    "return 0, 1, 1, 2, 3, 5, 8 list when asking 7 number" in {
      Basics.fib(7) must have length 7
      Basics.fib(7) must contain(0, 1, 1, 2, 3, 5, 8)
    }
  }
  "isSorted" should {
    def isInferior(a: Int, b:Int) =  a < b
    def isSizeInferior(a: String, b:String) =  a.length < b.length

    "return true for an empty array" in {
      Basics.isSorted[Int](Array(), isInferior) must beTrue
    }

    "return true for an array of size 1" in {
      Basics.isSorted[Int](Array(1), isInferior) must beTrue
    }

    "return false for unsorted array of int" in {
      Basics.isSorted[Int](Array(1,3,2), isInferior) must beFalse
    }

    "return true for sorted array of int" in {
      Basics.isSorted[Int](Array(1,2,3), isInferior) must beTrue
    }

    "return true for sorted array of string" in {
      Basics.isSorted[String](Array("q","qq","qqq"), isSizeInferior) must beTrue
    }

    "return false for sorted array of string" in {
      Basics.isSorted[String](Array("q","qqqq","qqq"), isSizeInferior) must beFalse
    }
  }
  "the curry function" should {
    "spice up the the function" in {
      Basics.curry[Int, String, Int]((a, b) => a + b.length )(1)("data") must_==(5)
    }
  }

  "the uncurry function" should {
    "mild the function" in {
      Basics.uncurry[String, Int, Int](a => b => a.length + b)("data", 1) must_==(5)
    }
  }

  "the compose function" should {
    "compose functions" in {
      Basics.compose[String, Int, Boolean](b => b % 2 == 0, a => a.length)("data") must beTrue
    }
  }
}
