package chapter6

trait RNG {
  def nextInt: (Int, RNG)
}

case class SimpleRNG(seed: Long) extends RNG
{
  def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }
}

object RNG {
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (rand, gen) = rng.nextInt
    if(rand == Int.MinValue){
      nonNegativeInt(gen)
    } else {
      (Math.abs(rand), gen)
    }
  }

  def double(rng: RNG): (Double, RNG) = {
    val (rand, gen) = nonNegativeInt(rng)
    (("0." + rand.toString).toDouble , gen)
  }

  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (int, gen1) = rng.nextInt
    val (double, gen2) = RNG.double(gen1)
    ((int, double ), gen2 )
  }

  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val (int, gen1) = rng.nextInt
    val (d, gen2) = double(gen1)
    ((d, int), gen2 )
  }

  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (d1, gen1) = double(rng)
    val (d2, gen2) = double(gen1)
    val (d3, gen3) = double(gen2)
    ((d1, d2, d3), gen3)
  }
}