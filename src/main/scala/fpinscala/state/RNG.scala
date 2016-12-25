package fpinscala.state

import com.sun.tools.doclint.HtmlTag.Flag

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

  type Rand[+A] = RNG => (A, RNG)

  def unit[A](a: A): Rand[A] = (rng) => (a, rng)

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = rng => {
    val (a, rng2) = f(rng)
    val (b, rng3) = g(a)(rng2)
    (b, rng3)
  }

  def nonNegativeLessThan(n: Int): Rand[Int] = flatMap(nonNegativeInt) {
    a =>
      val mod = a % n
      if (a + (n - 1) - mod >= 0) unit(mod)
      else nonNegativeLessThan(n)
  }

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] = flatMap(s) {
    a => unit(f(a))
  }

  val int: Rand[Int] = _.nextInt

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = flatMap(ra) {
    a => flatMap(rb) {
      b => unit(f(a,b))
    }
  }

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight(unit[List[A]](Nil))(map2(_, _)((x, xs) => x :: xs))


  def both[A,B](ra: Rand[A], rb: Rand[B]): Rand[(A,B)] = map2(ra, rb)((_, _))

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

  def doubleWithMap(rng: RNG): (Double, RNG) = map(nonNegativeInt) {
    int => int.toDouble / Int.MaxValue
  }(rng)

  def intDouble(rng: RNG) = randIntDouble(rng)

  def doubleInt(rng: RNG) = randDoubleInt(rng)

  val randIntDouble: Rand[(Int, Double)] = both(int, double)
  val randDoubleInt: Rand[(Double, Int)] = both(double, int)

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (v1, gen1) = double(rng)
    val (v2, gen2) = double(gen1)
    val (v3, gen3) = double(gen2)
    ((v1, v2, v3), gen3)
  }

  def rollDie: Rand[Int] = map(nonNegativeLessThan(6))(_ + 1)

  def ints(count: Int)(rng: RNG): (List[Int], RNG) =  sequence(List.fill(count)(int))(rng)
}