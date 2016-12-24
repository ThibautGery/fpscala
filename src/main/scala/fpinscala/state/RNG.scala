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
    val double = number / Int.MaxValue
    (double, gen)
  }
}