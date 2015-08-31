package chapter2

object IsSorted {
	def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = {
		def loop(n : Int): Boolean = {
			if(as.length == n + 2) ordered(as(n), as(n+1))
			else if(!ordered(as(n), as(n+1))) false
			else loop(n+1)
		}
		loop(0)
	}
}
