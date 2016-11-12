package part2

import scala.annotation.tailrec


object Basics {
  def fib(n: Int) : List[Int] = {
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

  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean) : Boolean = {
    @tailrec
    def go(ar:Array[A]): Boolean = {
      if(ar.isEmpty || ar.length == 1) true
      else if(!ordered(ar(0), ar(1))) false
      else go(ar.tail)
    }
    go(as)
  }

  def curry[A, B, C](f: (A, B) => C) : A => (B => C) = {
    (a: A) => (b: B) => f(a, b)
  }

  def uncurry[A, B, C](f: A => B => C): (A, B) => C = {
    (a, b) => f(a)(b) 
  }
}
