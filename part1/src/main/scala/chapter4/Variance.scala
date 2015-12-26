package chapter4


object Variance {
  def variance(xs: Seq[Double]): Option[Double] = {
    val m = xs.sum / xs. length
    def loop(xs: Seq[Double], acc :Option[Double]): Option[Double] = {
      xs match {
        case Nil =>
          acc
        case y :: ys =>
          loop(ys, acc.flatMap(sum => ecartType(y, m).map(_ + sum)))
      }
    }
    loop(xs, Some(0))
      .map(_ / xs.size)
  }

  def ecartType(elem : Double, mean: Double) : Option[Double] = {
    try {
      Some(math.pow(elem - mean, 2))
    } catch {
      case _:Exception => None
    }
  }
}
