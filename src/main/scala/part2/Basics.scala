package part2

import scala.annotation.tailrec


object Fibonacci {
  def fibonacci(n: Int) : List[Int] = {
    @tailrec
    def go(i : Int, acc:List[Int]): List[Int] = {
      if(i == 0) acc
      else {
        val n1 = acc(0)
        val n2 = acc(1)
        go(i - 1, (n1 + n2) :: acc)
      }
    }

    if(n == 0) Nil
    else if( n == 1) List(0)
    else go(n - 2, List(1, 0)).reverse
  }
}
