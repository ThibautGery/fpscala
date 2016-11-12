package part2

import org.specs2.mutable.Specification


class FibonacciTest extends Specification {
  "Fibonacci" should {
    "return an empty list when asking 0 number" in {
      Fibonacci.fibonacci(0) must be empty
    }

    "return 0 list when asking 1 number" in {
      Fibonacci.fibonacci(1) must have length 1
      Fibonacci.fibonacci(1) must contain(0)
    }

    "return 0 and 1 list when asking 2 number" in {
      Fibonacci.fibonacci(2) must have length 2
      Fibonacci.fibonacci(2) must contain(0, 1)
    }

    "return 0, 1, 1, 2, 3, 5, 8 list when asking 7 number" in {
      Fibonacci.fibonacci(7) must have length 7
      Fibonacci.fibonacci(7) must contain(0, 1, 1, 2, 3, 5, 8)
    }
  }
}
