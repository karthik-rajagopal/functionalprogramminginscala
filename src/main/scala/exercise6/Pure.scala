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

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (intNum, state1) = rng.nextInt
    val (doubleNum, state2) = double(state1)
    ((intNum, doubleNum), state2)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val ((i, d), newState) = intDouble(rng)
    ((d, i), newState)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (doubleNum1, state1) = double(rng)
    val (doubleNum2, state2) = double(state1)
    val (doubleNum3, state3) = double(state2)
    ((doubleNum1, doubleNum2, doubleNum3), state3)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    @tailrec
    def go(rands: List[Int], state: RNG, count: Int): (List[Int], RNG) = {
      if (count > 0) {
        val (i, newState) = state.nextInt
        go(i :: rands, newState, count - 1)
      } else {
        (rands, state)
      }
    }
    go(List.empty, rng, count)
  }
}


