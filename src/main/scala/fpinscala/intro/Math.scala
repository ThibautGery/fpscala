package fpinscala.intro

/**
  * Created by thibautgery on 1/12/16.
  */
object Math {
  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs).flatMap { m =>
      mean(xs.map(variancePerItem(m)))
    }



  def mean(xs: Seq[Double]): Option[Double] = {
    if(xs.isEmpty) None
    else Some(xs.sum / xs.size)
  }

  def variancePerItem(mean:Double)(item: Double): Double = math.pow(item - mean, 2)

}
