package chapter6

import scala.annotation.tailrec

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
  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] = rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def nonNegativeEven: Rand[Int] =
    map(nonNegativeInt)(i => i - i % 2)

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (rand, gen) = rng.nextInt
    if(rand == Int.MinValue){
      nonNegativeInt(gen)
    } else {
      (Math.abs(rand), gen)
    }
  }

  val double: Rand[Double] = map(nonNegativeInt)(rand => ("0." + rand.toString).toDouble)

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

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
  rng => {
    val (a, rng2) = ra(rng)
    val (b, rng3) = rb(rng2)
    (f(a, b), rng3)
  }


  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight[Rand[List[A]]](unit(Nil))((el, acc) => {
      map2(acc, el)((list, elem) => elem :: list )
    })

  def both[A,B](ra: Rand[A], rb: Rand[B]): Rand[(A,B)] =
    map2(ra, rb)((_, _))

  val randIntDouble: Rand[(Int, Double)] =
    both(int, double)
  val randDoubleInt: Rand[(Double, Int)] =
    both(double, int)

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    @tailrec
    def loop(count: Int, rng: RNG, acc: List[Int]) : (List[Int], RNG) = {
      if (count == 0) {
        (acc, rng)
      } else {
        val (rand, gen) = rng.nextInt
        loop(count - 1, gen, rand :: acc)
      }
    }
    loop(count, rng, Nil)
  }
}