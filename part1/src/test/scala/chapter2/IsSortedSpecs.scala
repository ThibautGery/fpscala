package chapter2

class IsSortedSpecs extends org.specs2.mutable.Specification {


  "[]" >> {
    "is sorted" >> {
      IsSorted.isSorted(Array(),(a: Int, b: Int) => a < b ) must beTrue
    }
  }

  "[0]" >> {
    "is sorted" >> {
      IsSorted.isSorted(Array(0),(a: Int, b: Int) => a < b ) must beTrue
    }
  }

  "[0, 1]" >> {
    "is sorted" >> {
      IsSorted.isSorted(Array(0, 1),(a: Int, b: Int) => a < b ) must beTrue
    }
  }


  "[0, 1, 5, 8 ,9]" >> {
    "is sorted" >> {
      IsSorted.isSorted(Array(0, 1, 5, 8 ,9),(a: Int, b: Int) => a < b ) must beTrue
    }
  }


  "[0, 1, 5, 8 ,9, 7]" >> {
    "is not sorted" >> {
      IsSorted.isSorted(Array(0, 1, 5, 8 ,9, 7),(a: Int, b: Int) => a < b ) must beFalse
    }
  }
}