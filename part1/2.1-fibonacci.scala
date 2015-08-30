def fibonacci(n: Int) : List[Int] = {

	@annotation.tailrec
	def go(current: Int, max: Int, acc: List[Int]) : List[Int] = {
		if (current == 0) go(current + 1, max,  Nil)
		else if (current > max) acc
		else if (current == 1) go(current + 1, max, 0 :: Nil)
		else if (current == 2) go(current + 1, max, 1 :: 0 :: Nil)
		else go(current +1, max, (acc.head +acc.tail.head) :: acc)
	}
	go(0, n, Nil).reverse

}
