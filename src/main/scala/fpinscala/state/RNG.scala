package fpinscala.state

trait RNG {
  def nextInt: (Int, RNG)
}

case class SimpleRNG(seed: Long) extends RNG {
  def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }
}

object RNG {
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (v, rng2) = rng.nextInt
    if(v == Int.MinValue) nonNegativeInt(rng2)
    else (Math.abs(v), rng2)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (number, gen) = nonNegativeInt(rng)
    val double = number.toDouble / Int.MaxValue
    (double, gen)
  }

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (intV, gen1) = rng.nextInt
    val (doubleV, gen2) = double(gen1)
    ((intV, doubleV), gen2)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val ((intV,doubleV), gen) = intDouble(rng)
    ((doubleV, intV), gen)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (v1, gen1) = double(rng)
    val (v2, gen2) = double(gen1)
    val (v3, gen3) = double(gen2)
    ((v1, v2, v3), gen3)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    (0 until count).foldLeft[(List[Int], RNG)]((Nil, rng))((acc, _) => {
      val (list, rng1): (List[Int], RNG) = acc
      val (int, rng2): (Int, RNG) = rng1.nextInt
      (int :: list, rng2)
    })
  }
}