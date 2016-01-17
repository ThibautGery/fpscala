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
    } else if(rand < 0) {
      (-rand, gen)
    } else {
      (rand, gen)
    }
  }

  def double(rng: RNG): (Double, RNG) = {
    val (rand, gen) = nonNegativeInt(rng)
    (("0." + rand.toString).toDouble , gen)
  }
}