package chapter2

/** This is the "Unit" style for specifications */
class FibonacciSpecs extends org.specs2.mutable.Specification {


  "Fibonacci of size 0" >> {
    "contain 0 elements" >> {
      Fibonacci.fibonacci(0) must haveSize(0)
    }
  }

  "Fibonacci of size 1" >> {
    "contain 1 elements" >> {
      Fibonacci.fibonacci(1) must haveSize(1)
    }

    "start with 0" >> {
      Fibonacci.fibonacci(1) must_== Seq(0)
    }
  }

  "Fibonacci of size 2" >> {
    "contain 2 elements" >> {
      Fibonacci.fibonacci(2) must haveSize(2)
    }

    "be 0, 1" >> {
      Fibonacci.fibonacci(2) must_== Seq(0, 1)
    }
  }

  "Fibonacci of size 5" >> {
    "contain 5 elements" >> {
      Fibonacci.fibonacci(5) must haveSize(5)
    }

    "be 0, 1, 1, 2, 3" >> {
      Fibonacci.fibonacci(5) must_== Seq(0, 1, 1, 2, 3)
    }
  }
}