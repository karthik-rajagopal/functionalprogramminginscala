package exercise6

import scala.annotation.tailrec

trait RNG {
  def nextInt: (Int, RNG)
}

case class SimpleRNG(seed: Long) extends RNG {
  override def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) * 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }
}

object RNG {
  def apply(seed: Long): RNG = SimpleRNG(seed)

  // function to generate a random integer between 0 and Int.Max (inclusive)
  def nonNegative(rng: RNG): (Int, RNG) = {
    val (n, newState) = rng.nextInt
    if (n < 0) {
      (-(n + 1), newState)
    } else {
      (n, newState)
    }
  }

  // generate a double between 0 and 1, not including 1
  def double(rng: RNG): (Double, RNG) = {
    val (n, newState) = nonNegative(rng)
    (n/Int.MaxValue.toDouble, newState)
  }
}


